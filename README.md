# Automata Visualiser

Automata Visualiser is a Haskell eDSL for creating images of various types of
automata. There is built in support for (D/N)FAs, PDAs, and Turing Machines,
with easy extensibililty available to add your own autamata types via the
`TransitionLabel` type class.

Automata images can be output to both an image format (SVGs) and a
LaTeX format (TikZ). Alternate output formats can be implemented by writing a
function `AutomatonConfig -> AutomatonLayout s t -> AutomatonRender`.

Automata Visualiser is available to use in an online playground at https://automata.codingcactus.codes

## Local Installation

The package isn't available on Hackage currently, but you can install it using
the GitHub link in the `cabal.project` file:

```cabal
packages: .

source-repository-package
    type: git
    location: https://github.com/Coding-Cactus/automata-visualiser

allow-newer: latex-svg-image:*
```

And then can be included as a dependency in your `.cabal` file as `automata-visualiser`.

## Syntax Documentation

Automata are defined with the `AutomatonBuilder` monad. You need to also specify
the types used for the state and transition labels. State labels must implement
the `Label` type class, and transition labels must implement the
`TransitionLabel` type class. These have been implemented for most common types.
For example, for an automaton with `String` state labels and `Int` transition labels, use
`AutomatonBuilder String Int`

To create new states, simply use the `state` function, passing the desired label as the
only parameter. To declare states as the initial state or as final states, use
the `initial` and `final` functions respectively.

To create transitions between states, an arrow syntax is used of the form
`state >--[transition label]--> state`. Multiple labels can be placed within the
square brackets separated by commas to attach multiple labels to a single
transition arrow in the produced diagram.

```hs
automaton :: AutomatonBuilder String Int
automaton = do
  a <- state "a"
  b <- state "b"

  initial a
  final b

  a >--[0]--> b
  b >--[0,1]--> b
```

### Extra Transition Types

- PDA transition labels can be created with the `(~~)` operator. For example: `0
  ~~ ('a', 'b')` represents the transition of seeing a `0` and popping `'a'` off
  the stack and pushing `'b'` onto the stack. The type of this transition label
  is `StackTransition Int Char`.
- Turing Machine labels can be created with the `TmT` constructor. For example
  `TmT 0 1 R` represents the transition of seeing a `0` on the tape, writing a
  `1` and then moving the head to the right. The type of this transition label
  is `TuringMachineTransition Int`.

Examples of these can be seen on https://automata.codingcactus.codes

### Manual Positioning

Automata Visualiser attempts to automatically position the states in an
aesthetically pleasing way. However, if wish to alter the layout manually, you
can do so with the following functions.

- `state1 \`above\` state2` positions `state1` directly above `state2`.
- Using the same syntax, the functions `leftOf`, `rightOf`, and `below` can also
  be used
- For complete control over state posiioning, the `position` function can be
  used. This function takes two states, an angle (in degrees), and a distance at
  which the states should be positioned. For example `position state1 state2 45
  2` will position `state2` at and angle of 45 degress from `state1`, at a
  distance of 2 units.

### Transition Shorthand

The `tr` and `tr'` functions can be used as shorter altnertives to the arrow
transition syntax. Recreating the above example using these functions looks can
be done as follows:

```hs
automaton :: AutomatonBuilder String Int
automaton = do
  a <- state "a"
  b <- state "b"

  initial a
  final b

  tr' a b 0
  tr a b [0,1]
```

## Configuration

There are a few configuration options available to tweak the appearance of the
diagrams. These can be set with the `AutomatonConfig` type. A default
configuration is available as `defaultConfig`.

```hs
config :: AutomatonConfig
config = setConfig = {
  acceptanceStyle = ..., -- DoubleCircle or Arrow
  svgStateRadius  = ..., -- Alter the state radius in SVG outputs by a multiplicative constant
  svgLoopRadius   = ..., -- Alter the self loop radius in SVG output by a multiplicative constant
  svgLoopSepAngle = ..., -- Alter the endpoint separation angle of self loop in SVG output by a multiplicative constant
  tikzLoopWidth   = ...  -- Alter the width of self loops in TikZ output by a multiplicative constant
}

```

## Generating Images

To finally generate the visual representation of your automata, the `render`
function can be used.

```hs
main :: IO ()
main = do
  render config fileName tikz automaton -- output to TikZ format
  render config fileName svg  automaton -- output to SVG format
```

## LaTex Support

Automata Visualiser supports LaTeX maths mode syntax in both state and transition labels.
For SVG output, your local LaTeX installation is used if available, and for TikZ
output: LaTeX syntax is trivially available.

If you don't have LaTeX installed locally, [the online playground](https://automata.codingcactus.codes) can be used.

