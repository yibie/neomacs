{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Rust toolchain (using rustup from system)
    rustup

    # Build tools
    pkg-config

    # Core dependencies
    glib
    cairo
    freetype
    fontconfig

    # EGL/OpenGL for wgpu
    libGL
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    libxkbcommon
    wayland

    # GStreamer
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-ugly
  ];

  # Set up environment for pkg-config
  PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
    pkgs.glib.dev
    pkgs.cairo.dev
    pkgs.freetype.dev
    pkgs.fontconfig.dev
    pkgs.libxkbcommon.dev
    pkgs.gst_all_1.gstreamer.dev
    pkgs.gst_all_1.gst-plugins-base.dev
  ];

  shellHook = ''
    echo "Neomacs display engine development environment"
    echo "Using rustup Rust toolchain"
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
      pkgs.glib
      pkgs.cairo
      pkgs.freetype
      pkgs.fontconfig
      pkgs.libGL
      pkgs.libxkbcommon
      pkgs.wayland
      pkgs.gst_all_1.gstreamer
      pkgs.gst_all_1.gst-plugins-base
    ]}:$LD_LIBRARY_PATH"
  '';
}
