/* Neomacs GPU-accelerated display backend implementation.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_NEOMACS

#include <dlfcn.h>
#include <string.h>
#include <gtk/gtk.h>

/* Forward declarations for Wayland types (to avoid including wayland headers) */
struct wl_display;

#include "lisp.h"
#include "blockinput.h"
#include "sysselect.h"
#include "neomacsterm.h"
#include "buffer.h"
#include "coding.h"
#include "window.h"
#include "keyboard.h"
#include "termhooks.h"
#include "termchar.h"
#include "font.h"
#include "pdumper.h"
#include "fontset.h"

/* List of Neomacs display info structures */
struct neomacs_display_info *neomacs_display_list = NULL;

/* Forward declarations */
extern frame_parm_handler neomacs_frame_parm_handlers[];

/* The redisplay interface for Neomacs frames - statically initialized.
   All function pointers are declared extern in neomacsterm.h */
static struct redisplay_interface neomacs_redisplay_interface = {
  .frame_parm_handlers = neomacs_frame_parm_handlers,
  .produce_glyphs = gui_produce_glyphs,
  .write_glyphs = gui_write_glyphs,
  .insert_glyphs = gui_insert_glyphs,
  .clear_end_of_line = gui_clear_end_of_line,
  .scroll_run_hook = neomacs_scroll_run,
  .after_update_window_line_hook = NULL,
  .update_window_begin_hook = NULL,
  .update_window_end_hook = NULL,
  .flush_display = neomacs_flush_display,
  .clear_window_mouse_face = gui_clear_window_mouse_face,
  .get_glyph_overhangs = gui_get_glyph_overhangs,
  .fix_overlapping_area = gui_fix_overlapping_area,
  .draw_fringe_bitmap = neomacs_draw_fringe_bitmap,
  .define_fringe_bitmap = NULL,
  .destroy_fringe_bitmap = NULL,
  .compute_glyph_string_overhangs = NULL,
  .draw_glyph_string = neomacs_draw_glyph_string,
  .define_frame_cursor = NULL,
  .clear_frame_area = neomacs_clear_frame_area,
  .clear_under_internal_border = NULL,
  .draw_window_cursor = neomacs_draw_window_cursor,
  .draw_vertical_window_border = NULL,
  .draw_window_divider = NULL,
  .shift_glyphs_for_insert = NULL,
  .show_hourglass = NULL,
  .hide_hourglass = NULL,
  .default_font_parameter = NULL,
};

/* Prototypes for internal functions */
static void neomacs_initialize_display_info (struct neomacs_display_info *);
static const char *neomacs_get_string_resource (void *, const char *, const char *);


/* ============================================================================
 * Display Initialization
 * ============================================================================ */

/* Initialize the Neomacs display subsystem */
void
neomacs_term_init (void)
{
  /* Initialize the Rust display engine */
  /* This will be called once at Emacs startup */
}

/* Create a new Neomacs display connection */
struct neomacs_display_info *
neomacs_open_display (const char *display_name)
{
  struct neomacs_display_info *dpyinfo;
  static bool gtk_initialized = false;

  /* Initialize GTK if not already done */
  if (!gtk_initialized)
    {
      gtk_init ();
      gtk_initialized = true;
    }

  dpyinfo = xzalloc (sizeof *dpyinfo);
  neomacs_initialize_display_info (dpyinfo);

  /* Initialize the Rust display engine */
  dpyinfo->display_handle = neomacs_display_init (BACKEND_TYPE_GTK4);

  if (!dpyinfo->display_handle)
    {
      xfree (dpyinfo);
      error ("Failed to initialize Neomacs display engine");
    }

  /* Add to display list */
  dpyinfo->next = neomacs_display_list;
  neomacs_display_list = dpyinfo;

  return dpyinfo;
}

/* Initialize display info defaults */
static void
neomacs_initialize_display_info (struct neomacs_display_info *dpyinfo)
{
  dpyinfo->reference_count = 0;
  dpyinfo->width = 800;
  dpyinfo->height = 600;
  dpyinfo->n_planes = 24;
  dpyinfo->black_pixel = 0x000000;
  dpyinfo->white_pixel = 0xffffff;
  dpyinfo->background_pixel = 0xffffff;
  dpyinfo->smallest_char_width = 8;
  dpyinfo->smallest_font_height = 16;
  dpyinfo->supports_argb = true;
  dpyinfo->connection = -1;
  dpyinfo->gdpy = NULL;

  /* Get the GDK display */
  GdkDisplay *gdpy = gdk_display_get_default ();
  if (!gdpy)
    {
      /* No display yet, try to open default */
      gdpy = gdk_display_open (NULL);
    }
  dpyinfo->gdpy = gdpy;

  if (gdpy)
    {
      /* Get the display connection fd for event handling.
         This depends on whether we're running on X11 or Wayland.
         Use dlsym to avoid compile-time dependencies on X11/Wayland headers.  */
      void *handle = dlopen (NULL, RTLD_LAZY);
      const char *type_name = G_OBJECT_TYPE_NAME (gdpy);

      if (0) fprintf (stderr, "DEBUG: GDK display type: %s\n", type_name ? type_name : "NULL");

      if (handle && type_name)
	{
	  /* Check for X11 display */
	  if (strstr (type_name, "X11"))
	    {
	      void *(*get_xdisplay) (GdkDisplay *) = dlsym (handle, "gdk_x11_display_get_xdisplay");
	      int (*conn_number) (void *) = dlsym (handle, "XConnectionNumber");
	      if (0) fprintf (stderr, "DEBUG: X11: get_xdisplay=%p, conn_number=%p\n",
		       (void *) get_xdisplay, (void *) conn_number);
	      if (get_xdisplay && conn_number)
		{
		  void *xdpy = get_xdisplay (gdpy);
		  if (0) fprintf (stderr, "DEBUG: X11: xdpy=%p\n", xdpy);
		  if (xdpy)
		    dpyinfo->connection = conn_number (xdpy);
		}
	    }
	  /* Check for Wayland display */
	  else if (strstr (type_name, "Wayland"))
	    {
	      struct wl_display *(*get_wl_display) (GdkDisplay *)
		= dlsym (handle, "gdk_wayland_display_get_wl_display");
	      int (*get_fd) (struct wl_display *) = dlsym (handle, "wl_display_get_fd");
	      if (0) fprintf (stderr, "DEBUG: Wayland: get_wl_display=%p, get_fd=%p\n",
		       (void *) get_wl_display, (void *) get_fd);
	      if (get_wl_display && get_fd)
		{
		  struct wl_display *wl_dpy = get_wl_display (gdpy);
		  if (0) fprintf (stderr, "DEBUG: Wayland: wl_dpy=%p\n", (void *) wl_dpy);
		  if (wl_dpy)
		    dpyinfo->connection = get_fd (wl_dpy);
		}
	    }
	}

      if (0) fprintf (stderr, "DEBUG: Display connection fd: %d\n", dpyinfo->connection);
    }
}


/* ============================================================================
 * Terminal Creation and Deletion
 * ============================================================================ */

/* Delete a Neomacs terminal */
void
neomacs_delete_terminal (struct terminal *terminal)
{
  struct neomacs_display_info *dpyinfo = terminal->display_info.neomacs;

  if (!dpyinfo)
    return;

  /* Shutdown the Rust display engine */
  if (dpyinfo->display_handle)
    {
      neomacs_display_shutdown (dpyinfo->display_handle);
      dpyinfo->display_handle = NULL;
    }

  /* Remove from display list */
  if (dpyinfo == neomacs_display_list)
    neomacs_display_list = dpyinfo->next;
  else
    {
      struct neomacs_display_info *tail;
      for (tail = neomacs_display_list; tail; tail = tail->next)
        if (tail->next == dpyinfo)
          {
            tail->next = dpyinfo->next;
            break;
          }
    }

  xfree (dpyinfo);
}

/* Create a terminal for a Neomacs display */
struct terminal *
neomacs_create_terminal (struct neomacs_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_neomacs, &neomacs_redisplay_interface);

  terminal->display_info.neomacs = dpyinfo;
  dpyinfo->terminal = terminal;

  terminal->name = xstrdup ("neomacs");

  /* Set up keyboard for this terminal */
  terminal->kboard = allocate_kboard (Qneomacs);
  /* Don't let the initial kboard remain current longer than necessary. */
  if (current_kboard == initial_kboard)
    current_kboard = terminal->kboard;
  terminal->kboard->reference_count++;

  /* Set up terminal hooks */
  terminal->delete_terminal_hook = neomacs_delete_terminal;
  terminal->update_begin_hook = neomacs_update_begin;
  terminal->update_end_hook = neomacs_update_end;
  terminal->defined_color_hook = neomacs_defined_color;
  terminal->get_string_resource_hook = neomacs_get_string_resource;
  terminal->set_new_font_hook = neomacs_new_font;
  terminal->read_socket_hook = neomacs_read_socket;

  /* Register the display connection fd for event handling */
  if (dpyinfo->connection >= 0)
    add_keyboard_wait_descriptor (dpyinfo->connection);

  /* More hooks would be set up here... */

  return terminal;
}


/* ============================================================================
 * Frame Update Hooks
 * ============================================================================ */

/* Called at the start of updating a frame */
void
neomacs_update_begin (struct frame *f)
{
  struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);

  if (dpyinfo && dpyinfo->display_handle)
    neomacs_display_begin_frame (dpyinfo->display_handle);
}

/* Called at the end of updating a frame */
void
neomacs_update_end (struct frame *f)
{
  struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);

  if (dpyinfo && dpyinfo->display_handle)
    neomacs_display_end_frame (dpyinfo->display_handle);

  /* Queue a redraw of the drawing area */
  if (output && output->drawing_area)
    gtk_widget_queue_draw (GTK_WIDGET (output->drawing_area));
}

/* Flush pending output to display */
void
neomacs_flush_display (struct frame *f)
{
  struct neomacs_output *output;

  if (!FRAME_NEOMACS_P (f))
    return;

  output = FRAME_NEOMACS_OUTPUT (f);

  /* Queue a redraw of the drawing area */
  if (output && output->drawing_area)
    gtk_widget_queue_draw (GTK_WIDGET (output->drawing_area));
}

/* Get a string resource value (for X resources / defaults) */
static const char *
neomacs_get_string_resource (void *rdb, const char *name, const char *class)
{
  /* Neomacs doesn't support X resources, return NULL */
  return NULL;
}


/* ============================================================================
 * Color Support
 * ============================================================================ */

/* Check if a color name is valid and return RGB values */
bool
neomacs_defined_color (struct frame *f, const char *color_name,
                       Emacs_Color *color_def, bool alloc, bool makeIndex)
{
  /* Simple color name parsing - expand as needed */
  if (!color_name || !color_def)
    return false;

  /* Try to parse common color names */
  if (strcmp (color_name, "black") == 0)
    {
      color_def->red = color_def->green = color_def->blue = 0;
      color_def->pixel = 0x000000;
      return true;
    }
  if (strcmp (color_name, "white") == 0)
    {
      color_def->red = color_def->green = color_def->blue = 65535;
      color_def->pixel = 0xffffff;
      return true;
    }

  /* Try to parse #RRGGBB format */
  if (color_name[0] == '#' && strlen (color_name) == 7)
    {
      unsigned int r, g, b;
      if (sscanf (color_name + 1, "%2x%2x%2x", &r, &g, &b) == 3)
        {
          color_def->red = r * 257;   /* Scale 0-255 to 0-65535 */
          color_def->green = g * 257;
          color_def->blue = b * 257;
          color_def->pixel = RGB_TO_ULONG (r, g, b);
          return true;
        }
    }

  return false;
}


/* ============================================================================
 * Cairo Helpers for Font Drivers
 * ============================================================================ */

/* Begin Cairo drawing with clipping */
cairo_t *
neomacs_begin_cr_clip (struct frame *f)
{
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);

  if (!output || !output->cr_context)
    return NULL;

  cairo_save (output->cr_context);
  return output->cr_context;
}

/* End Cairo drawing */
void
neomacs_end_cr_clip (struct frame *f)
{
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);

  if (output && output->cr_context)
    cairo_restore (output->cr_context);
}

/* Set Cairo source color */
void
neomacs_set_cr_source_with_color (struct frame *f, unsigned long color,
				  bool stipple_p)
{
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);

  if (!output || !output->cr_context)
    return;

  double r = RED_FROM_ULONG (color) / 255.0;
  double g = GREEN_FROM_ULONG (color) / 255.0;
  double b = BLUE_FROM_ULONG (color) / 255.0;

  cairo_set_source_rgb (output->cr_context, r, g, b);
}


/* ============================================================================
 * Text Drawing
 * ============================================================================ */

/* Draw a glyph string */
void
neomacs_draw_glyph_string (struct glyph_string *s)
{
  struct frame *f = s->f;
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);
  cairo_t *cr;

  if (!output || !output->cr_context)
    return;

  cr = output->cr_context;

  /* Set up clipping if needed */
  /* TODO: Handle clipping properly */

  /* Get face colors */
  unsigned long fg = s->face->foreground;
  unsigned long bg = s->face->background;

  /* Draw background if needed */
  if (!s->background_filled_p)
    {
      double r = RED_FROM_ULONG (bg) / 255.0;
      double g = GREEN_FROM_ULONG (bg) / 255.0;
      double b = BLUE_FROM_ULONG (bg) / 255.0;

      cairo_set_source_rgb (cr, r, g, b);
      cairo_rectangle (cr, s->x, s->y, s->background_width, s->height);
      cairo_fill (cr);
      s->background_filled_p = true;
    }

  /* Draw the foreground (text) */
  switch (s->first_glyph->type)
    {
    case CHAR_GLYPH:
    case COMPOSITE_GLYPH:
    case GLYPHLESS_GLYPH:
      {
	/* Set foreground color */
	double r = RED_FROM_ULONG (fg) / 255.0;
	double g = GREEN_FROM_ULONG (fg) / 255.0;
	double b = BLUE_FROM_ULONG (fg) / 255.0;
	cairo_set_source_rgb (cr, r, g, b);

	/* Draw using the font */
	if (s->font && s->nchars > 0 && s->char2b)
	  {
	    /* Use font's draw method if available */
	    struct font *font = s->font;

	    if (font->driver && font->driver->draw)
	      {
		font->driver->draw (s, 0, s->nchars, s->x, s->ybase, false);
	      }
	    else
	      {
		/* Fallback: Draw using Cairo text rendering */
		/* Convert char2b to string */
		char buf[256];
		int len = 0;
		for (int i = 0; i < s->nchars && len < 255; i++)
		  {
		    unsigned int c = s->char2b[i];
		    if (c < 128)
		      buf[len++] = (char) c;
		    else if (c < 0x800 && len < 254)
		      {
			buf[len++] = 0xC0 | (c >> 6);
			buf[len++] = 0x80 | (c & 0x3F);
		      }
		    /* Skip other multibyte for now */
		  }
		buf[len] = '\0';

		if (len > 0)
		  {
		    cairo_select_font_face (cr, "monospace",
					   CAIRO_FONT_SLANT_NORMAL,
					   CAIRO_FONT_WEIGHT_NORMAL);
		    cairo_set_font_size (cr, FRAME_LINE_HEIGHT (f) * 0.8);
		    cairo_move_to (cr, s->x, s->ybase);
		    cairo_show_text (cr, buf);
		  }
	      }
	  }
      }
      break;

    case STRETCH_GLYPH:
      /* Stretch glyphs are just background - already drawn above */
      break;

    case IMAGE_GLYPH:
      /* TODO: Implement image glyph drawing */
      break;

    default:
      break;
    }
}

/* Clear a rectangle on the frame */
void
neomacs_clear_frame_area (struct frame *f, int x, int y, int width, int height)
{
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);
  cairo_t *cr;

  if (!output || !output->cr_context)
    return;

  cr = output->cr_context;

  /* Get background color */
  unsigned long bg = output->background_pixel;
  double r = RED_FROM_ULONG (bg) / 255.0;
  double g = GREEN_FROM_ULONG (bg) / 255.0;
  double b = BLUE_FROM_ULONG (bg) / 255.0;

  cairo_set_source_rgb (cr, r, g, b);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
}

/* Draw fringe bitmap */
void
neomacs_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
                            struct draw_fringe_bitmap_params *p)
{
  /* TODO: Implement fringe bitmap drawing */
}


/* ============================================================================
 * Cursor Drawing
 * ============================================================================ */

/* Draw the cursor */
void
neomacs_draw_window_cursor (struct window *w, struct glyph_row *row,
                            int x, int y, enum text_cursor_kinds cursor_type,
                            int cursor_width, bool on_p, bool active_p)
{
  struct frame *f = XFRAME (w->frame);
  struct neomacs_output *output = FRAME_NEOMACS_OUTPUT (f);
  cairo_t *cr;

  if (!output || !output->cr_context || !on_p)
    return;

  cr = output->cr_context;

  /* Get cursor dimensions */
  int char_width = cursor_width > 0 ? cursor_width : FRAME_COLUMN_WIDTH (f);
  int char_height = FRAME_LINE_HEIGHT (f);

  /* Get cursor color - default to a visible color */
  unsigned long cursor_color = output->cursor_pixel;
  if (cursor_color == 0)
    cursor_color = 0x000000;  /* Black */

  double r = RED_FROM_ULONG (cursor_color) / 255.0;
  double g = GREEN_FROM_ULONG (cursor_color) / 255.0;
  double b = BLUE_FROM_ULONG (cursor_color) / 255.0;

  cairo_set_source_rgb (cr, r, g, b);

  switch (cursor_type)
    {
    case DEFAULT_CURSOR:
    case FILLED_BOX_CURSOR:
      /* Filled box cursor */
      cairo_rectangle (cr, x, y, char_width, char_height);
      cairo_fill (cr);
      break;

    case BAR_CURSOR:
      /* Vertical bar cursor */
      cairo_rectangle (cr, x, y, 2, char_height);
      cairo_fill (cr);
      break;

    case HBAR_CURSOR:
      /* Horizontal bar cursor */
      cairo_rectangle (cr, x, y + char_height - 2, char_width, 2);
      cairo_fill (cr);
      break;

    case HOLLOW_BOX_CURSOR:
      /* Hollow box cursor */
      cairo_set_line_width (cr, 1.0);
      cairo_rectangle (cr, x + 0.5, y + 0.5, char_width - 1, char_height - 1);
      cairo_stroke (cr);
      break;

    case NO_CURSOR:
      break;
    }
}


/* ============================================================================
 * Scrolling
 * ============================================================================ */

/* Scroll the contents of a window */
void
neomacs_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);

  if (!dpyinfo || !dpyinfo->display_handle)
    return;

  /* For smooth scrolling, use the animation API */
  /* neomacs_display_smooth_scroll (dpyinfo->display_handle, ...); */
}


/* ============================================================================
 * Exposure Handling
 * ============================================================================ */

/* Handle expose event - redraw the frame */
void
neomacs_expose_frame (struct frame *f)
{
  if (!FRAME_NEOMACS_P (f))
    return;

  /* Mark frame as needing redisplay */
  SET_FRAME_GARBAGED (f);
}

/* Called when frame is fully up to date */
void
neomacs_frame_up_to_date (struct frame *f)
{
  /* Nothing special needed */
}


/* ============================================================================
 * Focus Management
 * ============================================================================ */

/* Change focus to frame */
void
neomacs_focus_frame (struct frame *f, bool raise_flag)
{
  struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);

  if (!dpyinfo)
    return;

  dpyinfo->focus_frame = f;
}


/* ============================================================================
 * Cairo Integration for Font Rendering (ftcrfont.c)
 * ============================================================================ */

#include <cairo.h>


/* ============================================================================
 * Font Handling
 * ============================================================================ */

/* Set a new font for frame F.  This is called when a frame's font
   parameter is changed.  */
Lisp_Object
neomacs_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);
  int font_ascent, font_descent;

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;

  if (FRAME_FONT (f) == font)
    {
      /* This font is already set in frame F.  There's nothing more to do.  */
      return font_object;
    }

  FRAME_FONT (f) = font;

  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  get_font_ascent_descent (font, &font_ascent, &font_descent);
  FRAME_LINE_HEIGHT (f) = font_ascent + font_descent;

  /* We could use a more elaborate calculation here.  */
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid - 1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Compute the scroll bar height in character lines.  */
  if (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0)
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f)
	= (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) + height - 1) / height;
    }
  else
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f) = (14 + height - 1) / height;
    }

  /* Invalidate face cache since fonts changed.  */
  if (FRAME_FACE_CACHE (f))
    {
      struct face_cache *c = FRAME_FACE_CACHE (f);
      if (c)
	c->used = 0;
    }

  return font_object;
}


/* ============================================================================
 * Input Event Handling
 * ============================================================================ */

/* Read socket events for the Neomacs terminal.
   This processes GTK4 events and converts them to Emacs events.  */
int
neomacs_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  GMainContext *context;
  int count = 0;
  static int call_count = 0;

  call_count++;
  if (call_count <= 5 || (call_count % 100 == 0))
    if (0) fprintf (stderr, "DEBUG: neomacs_read_socket called, count=%d\n", call_count);

  /* Get the default GLib main context */
  context = g_main_context_default ();

  block_input ();

  /* Process pending GTK events using iteration
     - FALSE means don't block if no events */
  while (g_main_context_iteration (context, FALSE))
    {
      /* Events were processed */
    }

  unblock_input ();

  return count;
}


/* ============================================================================
 * Redisplay Interface
 * ============================================================================ */


/* ============================================================================
 * Lisp Interface
 * ============================================================================ */

DEFUN ("neomacs-available-p", Fneomacs_available_p, Sneomacs_available_p, 0, 0, 0,
       doc: /* Return t if Neomacs display backend is available.  */)
  (void)
{
  return Qt;
}

DEFUN ("neomacs-display-list", Fneomacs_display_list, Sneomacs_display_list, 0, 0, 0,
       doc: /* Return a list of all Neomacs display connections.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct neomacs_display_info *dpyinfo;

  for (dpyinfo = neomacs_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
      if (dpyinfo->terminal)
        result = Fcons (make_fixnum (dpyinfo->terminal->id), result);
    }

  return result;
}

DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
  (void)
{
  /* TODO: Implement tooltip hiding */
  return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Return t if the display supports color.  */)
  (Lisp_Object terminal)
{
  /* Neomacs always supports full color via GTK4 */
  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p, 0, 1, 0,
       doc: /* Return t if the display can show shades of gray.  */)
  (Lisp_Object terminal)
{
  /* Neomacs displays support both color and grayscale */
  return Qnil;  /* Return nil meaning we support full color, not just grayscale */
}


/* ============================================================================
 * Miscellaneous Functions
 * ============================================================================ */

/* Called from frame.c to get display info for x-get-resource.  */
struct neomacs_display_info *
check_x_display_info (Lisp_Object frame)
{
  struct frame *f;

  if (NILP (frame))
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  if (!FRAME_NEOMACS_P (f))
    error ("Frame is not a Neomacs frame");

  return FRAME_NEOMACS_DISPLAY_INFO (f);
}

/* Get a human-readable name for a keysym.  */
char *
get_keysym_name (int keysym)
{
  /* For GTK4, we could use gdk_keyval_name, but for now return NULL */
  /* This function is used for debugging and error messages */
  return NULL;
}

/* Set mouse pixel position on frame F.  */
void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
  /* TODO: Implement with GTK4 */
}


/* ============================================================================
 * Toolbar Support
 * ============================================================================ */

/* Update the tool bar for frame F.  Currently a stub.  */
void
update_frame_tool_bar (struct frame *f)
{
  /* TODO: Implement tool bar update via Rust/GTK4 */
}

/* Free the tool bar resources for frame F.  Currently a stub.  */
void
free_frame_tool_bar (struct frame *f)
{
  /* TODO: Implement tool bar cleanup */
}


/* ============================================================================
 * Menu Bar Support
 * ============================================================================ */

/* Update the menu bar for frame F.  Currently a stub.  */
void
set_frame_menubar (struct frame *f, bool deep_p)
{
  /* TODO: Implement menu bar via Rust/GTK4 */
}

/* Free the menu bar resources for frame F.  Currently a stub.  */
void
free_frame_menubar (struct frame *f)
{
  /* TODO: Implement menu bar cleanup */
}


/* ============================================================================
 * Initialization
 * ============================================================================ */

void
syms_of_neomacsterm (void)
{
  /* Redisplay interface is now statically initialized */

  defsubr (&Sneomacs_available_p);
  defsubr (&Sneomacs_display_list);
  defsubr (&Sx_hide_tip);
  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);

  DEFSYM (Qneomacs, "neomacs");

  /* Required variables for cus-start */
  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_use_underline_position_properties = 1;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_underline_at_descent_line = 0;

  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
     doc: /* SKIP: real doc in xterm.c.  */);
  Vx_toolkit_scroll_bars = intern_c_string ("gtk");

  DEFVAR_LISP ("x-ctrl-keysym", Vx_ctrl_keysym,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_ctrl_keysym = Qnil;
  DEFVAR_LISP ("x-alt-keysym", Vx_alt_keysym,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_alt_keysym = Qnil;
  DEFVAR_LISP ("x-hyper-keysym", Vx_hyper_keysym,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_hyper_keysym = Qnil;
  DEFVAR_LISP ("x-meta-keysym", Vx_meta_keysym,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_meta_keysym = Qnil;
  DEFVAR_LISP ("x-super-keysym", Vx_super_keysym,
	       doc: /* SKIP: real doc in xterm.c.  */);
  Vx_super_keysym = Qnil;

  /* Tell Emacs about this window system */
  Fprovide (Qneomacs, Qnil);
}

#endif /* HAVE_NEOMACS */
