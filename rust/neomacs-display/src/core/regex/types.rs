//! Core types for the Emacs regex engine.

/// Regex opcodes — the bytecode instruction set.
///
/// This matches the semantics of Emacs's `re_opcode_t` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    /// No operation (padding).
    NoOp = 0,
    /// Match succeeded.
    Succeed = 1,
    /// Match exactly N bytes (followed by count byte + literal bytes).
    Exactn = 2,
    /// Match any character except newline.
    AnyChar = 3,
    /// Character set (bitmap for ASCII + optional range table for multibyte).
    Charset = 4,
    /// Negated character set.
    CharsetNot = 5,
    /// Start of capture group N (followed by group number byte).
    StartMemory = 6,
    /// End of capture group N (followed by group number byte).
    StopMemory = 7,
    /// Backreference to group N (followed by group number byte).
    Duplicate = 8,
    /// Match at line start (beginning of string or after \n).
    BegLine = 9,
    /// Match at line end (end of string or before \n).
    EndLine = 10,
    /// Match at buffer/string start.
    BegBuf = 11,
    /// Match at buffer/string end.
    EndBuf = 12,
    /// Unconditional jump (2-byte signed relative offset).
    Jump = 13,
    /// Push failure point, continue (2-byte offset to failure target).
    OnFailureJump = 14,
    /// Like OnFailureJump but don't restore string position on failure.
    OnFailureKeepStringJump = 15,
    /// Loop jump with cycle detection.
    OnFailureJumpLoop = 16,
    /// Non-greedy loop with cycle detection.
    OnFailureJumpNastyloop = 17,
    /// Self-modifying greedy loop optimization.
    OnFailureJumpSmart = 18,
    /// Succeed N times (4 bytes: 2-byte offset + 2-byte counter).
    SucceedN = 19,
    /// Jump N times (4 bytes: 2-byte offset + 2-byte counter).
    JumpN = 20,
    /// Modify counter at offset (4 bytes: 2-byte offset + 2-byte value).
    SetNumberAt = 21,
    /// Word start boundary.
    WordBeg = 22,
    /// Word end boundary.
    WordEnd = 23,
    /// Word boundary (\b).
    WordBound = 24,
    /// Non-word boundary (\B).
    NotWordBound = 25,
    /// Symbol start boundary.
    SymBeg = 26,
    /// Symbol end boundary.
    SymEnd = 27,
    /// Match character with given syntax class (followed by syntax code byte).
    SyntaxSpec = 28,
    /// Negated syntax class.
    NotSyntaxSpec = 29,
    /// Match character in category (followed by category code byte).
    CategorySpec = 30,
    /// Negated category.
    NotCategorySpec = 31,
    /// Match at point (Emacs buffer-specific).
    AtDot = 32,
}

impl Opcode {
    pub fn from_u8(val: u8) -> Option<Self> {
        if val <= 32 {
            Some(unsafe { std::mem::transmute(val) })
        } else {
            None
        }
    }
}

/// Emacs syntax classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SyntaxClass {
    Whitespace = 0,
    Punct = 1,
    Word = 2,
    Symbol = 3,
    Open = 4,
    Close = 5,
    Quote = 6,
    String = 7,
    Math = 8,
    Escape = 9,
    CharQuote = 10,
    Comment = 11,
    EndComment = 12,
    InheritStandard = 13,
    CommentFence = 14,
    StringFence = 15,
    Max = 16,
}

/// Named character classes for [:alpha:], [:digit:], etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharClass {
    Alnum,
    Alpha,
    Blank,
    Cntrl,
    Digit,
    Graph,
    Lower,
    Print,
    Punct,
    Space,
    Upper,
    XDigit,
    Ascii,
    NonAscii,
    Word,
    Multibyte,
    Unibyte,
}

impl CharClass {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "alnum" => Some(CharClass::Alnum),
            "alpha" => Some(CharClass::Alpha),
            "blank" => Some(CharClass::Blank),
            "cntrl" => Some(CharClass::Cntrl),
            "digit" => Some(CharClass::Digit),
            "graph" => Some(CharClass::Graph),
            "lower" => Some(CharClass::Lower),
            "print" => Some(CharClass::Print),
            "punct" => Some(CharClass::Punct),
            "space" => Some(CharClass::Space),
            "upper" => Some(CharClass::Upper),
            "xdigit" => Some(CharClass::XDigit),
            "ascii" => Some(CharClass::Ascii),
            "nonascii" => Some(CharClass::NonAscii),
            "word" => Some(CharClass::Word),
            "multibyte" => Some(CharClass::Multibyte),
            "unibyte" => Some(CharClass::Unibyte),
            _ => None,
        }
    }

    /// Test if a character matches this class.
    pub fn matches(&self, c: char) -> bool {
        let cu = c as u32;
        match self {
            CharClass::Alnum => c.is_alphanumeric(),
            CharClass::Alpha => c.is_alphabetic(),
            CharClass::Blank => c == ' ' || c == '\t',
            CharClass::Cntrl => c.is_control(),
            CharClass::Digit => c.is_ascii_digit(),
            CharClass::Graph => !c.is_control() && !c.is_whitespace() && cu > 0x20,
            CharClass::Lower => c.is_lowercase(),
            CharClass::Print => !c.is_control() || c == ' ',
            CharClass::Punct => c.is_ascii_punctuation() || (cu > 127 && !c.is_alphanumeric() && !c.is_whitespace()),
            CharClass::Space => c.is_whitespace(),
            CharClass::Upper => c.is_uppercase(),
            CharClass::XDigit => c.is_ascii_hexdigit(),
            CharClass::Ascii => cu < 128,
            CharClass::NonAscii => cu >= 128,
            CharClass::Word => c.is_alphanumeric() || c == '_',
            CharClass::Multibyte => cu >= 128,
            CharClass::Unibyte => cu < 256,
        }
    }
}

/// Compiled regex pattern buffer.
#[derive(Clone)]
pub struct PatternBuffer {
    /// Compiled bytecode.
    pub bytecode: Vec<u8>,
    /// Number of capture groups (subexpressions).
    pub num_groups: usize,
    /// Whether the pattern can match an empty string.
    pub can_be_null: bool,
    /// 256-byte fastmap for first-character optimization.
    /// Entry `i` is true if byte `i` can be the first byte of a match.
    pub fastmap: [bool; 256],
    /// Whether the fastmap is valid.
    pub fastmap_accurate: bool,
    /// Whether the pattern was compiled with multibyte support.
    pub multibyte: bool,
    /// Whether the pattern uses syntax table features.
    pub uses_syntax: bool,
}

impl PatternBuffer {
    pub fn new() -> Self {
        PatternBuffer {
            bytecode: Vec::new(),
            num_groups: 0,
            can_be_null: false,
            fastmap: [false; 256],
            fastmap_accurate: false,
            multibyte: true,
            uses_syntax: false,
        }
    }
}

impl Default for PatternBuffer {
    fn default() -> Self {
        Self::new()
    }
}

/// Capture group register results.
#[derive(Debug, Clone)]
pub struct Registers {
    /// Number of registers (groups + 1 for whole match).
    pub num_regs: usize,
    /// Start byte positions for each register (-1 if unmatched).
    pub starts: Vec<i64>,
    /// End byte positions for each register (-1 if unmatched).
    pub ends: Vec<i64>,
}

impl Registers {
    pub fn new(num_regs: usize) -> Self {
        Registers {
            num_regs,
            starts: vec![-1; num_regs],
            ends: vec![-1; num_regs],
        }
    }

    pub fn reset(&mut self) {
        for s in &mut self.starts {
            *s = -1;
        }
        for e in &mut self.ends {
            *e = -1;
        }
    }
}

/// Compilation error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegexError {
    InvalidPattern(String),
    TrailingBackslash,
    InvalidBackreference(u8),
    UnmatchedBracket,
    UnmatchedParen,
    UnmatchedBrace,
    InvalidRepetition(String),
    InvalidRange,
    InvalidCharClass(String),
    PatternTooLarge,
    OutOfMemory,
    NoPrecedingElement,
}

impl std::fmt::Display for RegexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegexError::InvalidPattern(s) => write!(f, "Invalid pattern: {}", s),
            RegexError::TrailingBackslash => write!(f, "Trailing backslash"),
            RegexError::InvalidBackreference(n) => write!(f, "Invalid backreference \\{}", n),
            RegexError::UnmatchedBracket => write!(f, "Unmatched ["),
            RegexError::UnmatchedParen => write!(f, "Unmatched \\("),
            RegexError::UnmatchedBrace => write!(f, "Unmatched \\{{"),
            RegexError::InvalidRepetition(s) => write!(f, "Invalid repetition: {}", s),
            RegexError::InvalidRange => write!(f, "Invalid range"),
            RegexError::InvalidCharClass(s) => write!(f, "Invalid character class: {}", s),
            RegexError::PatternTooLarge => write!(f, "Pattern too large"),
            RegexError::OutOfMemory => write!(f, "Out of memory"),
            RegexError::NoPrecedingElement => write!(f, "No preceding element for repetition"),
        }
    }
}

impl std::error::Error for RegexError {}

/// Match result.
#[derive(Debug, Clone)]
pub enum MatchResult {
    /// Match found at the given byte position.
    Match(i64),
    /// No match.
    NoMatch,
    /// Internal error.
    Error,
}

/// Callback trait for syntax/category table integration.
/// This allows the regex engine to query Emacs-specific character properties
/// without depending on Emacs data structures.
pub trait CharProperties {
    /// Return the syntax class of character `c` at position `pos`.
    fn syntax_class(&self, c: char, pos: usize) -> SyntaxClass;

    /// Return true if character `c` belongs to category `cat`.
    fn in_category(&self, c: char, cat: u8) -> bool;

    /// Translate a character for case-folding. Returns the canonical form.
    fn translate(&self, c: char) -> char;
}

/// Default implementation that uses Unicode properties.
pub struct DefaultCharProperties;

impl CharProperties for DefaultCharProperties {
    fn syntax_class(&self, c: char, _pos: usize) -> SyntaxClass {
        if c.is_whitespace() {
            SyntaxClass::Whitespace
        } else if c.is_alphanumeric() || c == '_' {
            SyntaxClass::Word
        } else if c == '(' || c == '[' || c == '{' {
            SyntaxClass::Open
        } else if c == ')' || c == ']' || c == '}' {
            SyntaxClass::Close
        } else if c == '"' {
            SyntaxClass::String
        } else if c == '\\' {
            SyntaxClass::Escape
        } else {
            SyntaxClass::Punct
        }
    }

    fn in_category(&self, _c: char, _cat: u8) -> bool {
        false
    }

    fn translate(&self, c: char) -> char {
        c
    }
}
