;; coding: utf-8

(define-module (xile xi-protocol themes)
  #:use-module (srfi srfi-9)
  #:export (make-xile-theme
            xile-theme-name
            xile-theme-settings
            parse-syntect-theme-settings
            syntext-theme-settings-foreground
            syntext-theme-settings-background
            syntext-theme-settings-caret
            syntext-theme-settings-line-highlight
            syntext-theme-settings-misspelling
            syntext-theme-settings-minimap-border
            syntext-theme-settings-accent
            syntext-theme-settings-popup-css
            syntext-theme-settings-phantom-css
            syntext-theme-settings-bracket-contents-foreground
            syntext-theme-settings-bracket-contents-options
            syntext-theme-settings-brackets-foreground
            syntext-theme-settings-brackets-background
            syntext-theme-settings-brackets-options
            syntext-theme-settings-tags-foreground
            syntext-theme-settings-tags-options
            syntext-theme-settings-highlight
            syntext-theme-settings-find-highlight
            syntext-theme-settings-find-highlight-foreground
            syntext-theme-settings-gutter
            syntext-theme-settings-gutter-foreground
            syntext-theme-settings-selection
            syntext-theme-settings-selection-foreground
            syntext-theme-settings-selection-background
            syntext-theme-settings-selection-border
            syntext-theme-settings-inactive-selection
            syntext-theme-settings-inactive-selection-foreground
            syntext-theme-settings-guide
            syntext-theme-settings-active-guide
            syntext-theme-settings-stack-guide
            syntext-theme-settings-highlight-foreground
            syntext-theme-settings-shadow
            syntect-color-r
            syntect-color-g
            syntect-color-b
            syntect-color-a
            syntect-underline-option-type))

(define-record-type <syntect-color>
  (make-syntect-color red green blue alpha)
  syntect-color?
  (red syntect-color-r)
  (green syntect-color-g)
  (blue syntect-color-b)
  (alpha syntect-color-a))

(define (parse-syntect-color result)
  "Parse a deserialized json RESULT into a syntect-color record.

/// RGBA color, these numbers come directly from the theme so
/// for now you might have to do your own color space conversion if you are outputting
/// a different color space from the theme. This can be a problem because some Sublime
/// themes use sRGB and some don't. This is specified in an attribute syntect doesn't parse yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Color {
    /// Red component
    pub r: u8,
    /// Green component
    pub g: u8,
    /// Blue component
    pub b: u8,
    /// Alpha component
    pub a: u8,
}"
  (let ((red (assoc-ref result "r"))
        (green (assoc-ref result "g"))
        (blue (assoc-ref result "b"))
        (alpha (assoc-ref result "a")))
    (make-syntect-color red green blue alpha)))

(define-record-type <syntect-underline-option>
  (make-syntect-underline-option type)
  syntect-underline-option?
  (type syntect-underline-option-type))

(define (parse-syntect-underline-option option)
  "Parse a deserialized underline option into a syntect-underline-option record.
Checks that the type is correct, none if unknown.

pub enum UnderlineOption {
    None,
    Underline,
    StippledUnderline,
    SquigglyUnderline,
}
"
  (cond ((string=? option "Underline")
         (make-syntect-underline-option 'underline))
        ((string=? option "StippledUnderline")
         (make-syntect-underline-option 'stippled-underline))
        ((string=? option "SquigglyUnderline")
         (make-syntect-underline-option 'squiggly-underline))
        (#t (make-syntect-underline-option 'none))))


(define-record-type <syntect-theme-settings>
  (make-syntect-theme-settings foreground background caret line-highlight
                               misspelling minimap-border accent popup-css phantom-css
                               bracket-contents-foreground bracket-contents-options brackets-foreground
                               brackets-background brackets-options tags-foreground tags-options highlight
                               find-highlight find-highlight-foreground gutter gutter-foreground selection
                               selection-foreground selection-background selection-border inactive-selection
                               inactive-selection-foreground guide active-guide stack-guide
                               highlight-foreground shadow)
  syntect-theme-settings?
  (foreground syntext-theme-settings-foreground)
  (background syntext-theme-settings-background)
  (caret syntext-theme-settings-caret)
  (line-highlight syntext-theme-settings-line-highlight)
  (misspelling syntext-theme-settings-misspelling)
  (minimap-border syntext-theme-settings-minimap-border)
  (accent syntext-theme-settings-accent)
  (popup-css syntext-theme-settings-popup-css)
  (phantom-css syntext-theme-settings-phantom-css)
  (bracket-contents-foreground syntext-theme-settings-bracket-contents-foreground)
  (bracket-contents-options syntext-theme-settings-bracket-contents-options)
  (brackets-foreground syntext-theme-settings-brackets-foreground)
  (brackets-background syntext-theme-settings-brackets-background)
  (brackets-options syntext-theme-settings-brackets-options)
  (tags-foreground syntext-theme-settings-tags-foreground)
  (tags-options syntext-theme-settings-tags-options)
  (highlight syntext-theme-settings-highlight)
  (find-highlight syntext-theme-settings-find-highlight)
  (find-highlight-foreground syntext-theme-settings-find-highlight-foreground)
  (gutter syntext-theme-settings-gutter)
  (gutter-foreground syntext-theme-settings-gutter-foreground)
  (selection syntext-theme-settings-selection)
  (selection-foreground syntext-theme-settings-selection-foreground)
  (selection-background syntext-theme-settings-selection-background)
  (selection-border syntext-theme-settings-selection-border)
  (inactive-selection syntext-theme-settings-inactive-selection)
  (inactive-selection-foreground syntext-theme-settings-inactive-selection-foreground)
  (guide syntext-theme-settings-guide)
  (active-guide syntext-theme-settings-active-guide)
  (stack-guide syntext-theme-settings-stack-guide)
  (highlight-foreground syntext-theme-settings-highlight-foreground)
  (shadow syntext-theme-settings-shadow))

(define (parse-syntect-theme-settings result)
  "Parse a deserialized json RESULT into a syntect-theme-settings record.

/// Various properties meant to be used to style a text editor.
/// Basically all the styles that aren't directly applied to text like selection color.
/// Use this to make your editor UI match the highlighted text.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ThemeSettings {
    /// The default color for text.
    pub foreground: Option<Color>,
    /// The default backgound color of the view.
    pub background: Option<Color>,
    /// Color of the caret.
    pub caret: Option<Color>,
    /// Color of the line the caret is in.
    /// Only used when the `higlight_line` setting is set to `true`.
    pub line_highlight: Option<Color>,

    /// The color to use for the squiggly underline drawn under misspelled words.
    pub misspelling: Option<Color>,
    /// The color of the border drawn around the viewport area of the minimap.
    /// Only used when the `draw_minimap_border` setting is enabled.
    pub minimap_border: Option<Color>,
    /// A color made available for use by the theme.
    pub accent: Option<Color>,
    /// CSS passed to popups.
    pub popup_css: Option<String>,
    /// CSS passed to phantoms.
    pub phantom_css: Option<String>,

    /// Color of bracketed sections of text when the caret is in a bracketed section.
    /// Only applied when the `match_brackets` setting is set to `true`.
    pub bracket_contents_foreground: Option<Color>,
    /// Controls certain options when the caret is in a bracket section.
    /// Only applied when the `match_brackets` setting is set to `true`.
    pub bracket_contents_options: Option<UnderlineOption>,
    /// Foreground color of the brackets when the caret is next to a bracket.
    /// Only applied when the `match_brackets` setting is set to `true`.
    pub brackets_foreground: Option<Color>,
    /// Background color of the brackets when the caret is next to a bracket.
    /// Only applied when the `match_brackets` setting is set to `true`.
    pub brackets_background: Option<Color>,
    /// Controls certain options when the caret is next to a bracket.
    /// Only applied when the match_brackets setting is set to `true`.
    pub brackets_options: Option<UnderlineOption>,

    /// Color of tags when the caret is next to a tag.
    /// Only used when the `match_tags` setting is set to `true`.
    pub tags_foreground: Option<Color>,
    /// Controls certain options when the caret is next to a tag.
    /// Only applied when the match_tags setting is set to `true`.
    pub tags_options: Option<UnderlineOption>,

    /// The border color for \"other\" matches.
    pub highlight: Option<Color>,
    /// Background color of regions matching the current search.
    pub find_highlight: Option<Color>,
    /// Text color of regions matching the current search.
    pub find_highlight_foreground: Option<Color>,

    /// Background color of the gutter.
    pub gutter: Option<Color>,
    /// Foreground color of the gutter.
    pub gutter_foreground: Option<Color>,

    /// The background color of selected text.
    pub selection: Option<Color>,
    /// A color that will override the scope-based text color of the selection.
    pub selection_foreground: Option<Color>,

    /// Deprecated!
    ///
    /// This property is not part of the recognized tmTheme format. It may be
    /// removed in a future release.
    pub selection_background: Option<Color>,

    /// Color of the selection regions border.
    pub selection_border: Option<Color>,
    /// The background color of a selection in a view that is not currently focused.
    pub inactive_selection: Option<Color>,
    /// A color that will override the scope-based text color of the selection
    /// in a view that is not currently focused.
    pub inactive_selection_foreground: Option<Color>,

    /// Color of the guides displayed to indicate nesting levels.
    pub guide: Option<Color>,
    /// Color of the guide lined up with the caret.
    /// Only applied if the `indent_guide_options` setting is set to `draw_active`.
    pub active_guide: Option<Color>,
    /// Color of the current guide’s parent guide level.
    /// Only used if the `indent_guide_options` setting is set to `draw_active`.
    pub stack_guide: Option<Color>,

    /// Foreground color for regions added via `sublime.add_regions()`
    /// with the `sublime.DRAW_OUTLINED` flag added.
    ///
    /// Deprecated!
    /// This setting does not exist in any available documentation.
    /// Use is discouraged, and it may be removed in a future release.
    pub highlight_foreground: Option<Color>,

    /// The color of the shadow used when a text area can be horizontally scrolled.
    pub shadow: Option<Color>,
} "
  (make-syntect-theme-settings
   (and=> (assoc-ref result "foreground") parse-syntect-color)
   (and=> (assoc-ref result "background") parse-syntect-color)
   (and=> (assoc-ref result "caret") parse-syntect-color)
   (and=> (assoc-ref result "line_highlight") parse-syntect-color)
   (and=> (assoc-ref result "misspelling") parse-syntect-color)
   (and=> (assoc-ref result "minimap_border") parse-syntect-color)
   (and=> (assoc-ref result "accent") parse-syntect-color)
   (or (assoc-ref result "popup_css") "")
   (or (assoc-ref result "phantom_css") "")
   (and=> (assoc-ref result "bracket_contents_foreground") parse-syntect-color)
   (and=> (assoc-ref result "bracket_contents_options") parse-syntect-underline-option)
   (and=> (assoc-ref result "brackets_foreground") parse-syntect-color)
   (and=> (assoc-ref result "brackets_background") parse-syntect-color)
   (and=> (assoc-ref result "brackets_options") parse-syntect-underline-option)
   (and=> (assoc-ref result "tags_foreground") parse-syntect-color)
   (and=> (assoc-ref result "tags_options") parse-syntect-underline-option)
   (and=> (assoc-ref result "highlight") parse-syntect-color)
   (and=> (assoc-ref result "find_highlight") parse-syntect-color)
   (and=> (assoc-ref result "find_highlight_foreground") parse-syntect-color)
   (and=> (assoc-ref result "gutter") parse-syntect-color)
   (and=> (assoc-ref result "gutter_foreground") parse-syntect-color)
   (and=> (assoc-ref result "selection") parse-syntect-color)
   (and=> (assoc-ref result "selection_foreground") parse-syntect-color)
   (and=> (assoc-ref result "selection_background") parse-syntect-color)
   (and=> (assoc-ref result "selection_border") parse-syntect-color)
   (and=> (assoc-ref result "inactive_selection") parse-syntect-color)
   (and=> (assoc-ref result "inactive_selection_foreground") parse-syntect-color)
   (and=> (assoc-ref result "guide") parse-syntect-color)
   (and=> (assoc-ref result "active_guide") parse-syntect-color)
   (and=> (assoc-ref result "stack_guide") parse-syntect-color)
   (and=> (assoc-ref result "highlight_foreground") parse-syntect-color)
   (and=> (assoc-ref result "shadow") parse-syntect-color)))

(define-record-type <xile-theme>
  (make-xile-theme name settings)
  xile-theme?
  (name xile-theme-name)
  (settings xile-theme-settings))
