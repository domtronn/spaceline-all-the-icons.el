<p align="center">
<img src="logo.png" width="1200">
</p>

<p align="center">
<b><a href="#installation">Installation</a></b>
|
<b><a href="#usage">Usage</a></b>
|
<b><a href="#customization">Customization</a></b>
|
<b><a href="#debugging">Debugging</a></b>
</p>

<p align="center">
  <a href="https://melpa.org/#/spaceline-all-the-icons">
    <img src="https://melpa.org/packages/spaceline-all-the-icons-badge.svg" />
  </a>
  <a href="https://stable.melpa.org/#/spaceline-all-the-icons">
    <img src="https://stable.melpa.org/packages/spaceline-all-the-icons-badge.svg" />
  </a>
  <a href="https://github.com/domtronn/spaceline-all-the-icons.el/releases">
    <img src="https://img.shields.io/github/tag/domtronn/spaceline-all-the-icons.el.svg" />
  </a>
  <a href="https://github.com/domtronn/spaceline-all-the-icons.el/issues">
    <img src="https://img.shields.io/issuestats/i/github/domtronn/spaceline-all-the-icons.el.svg" />
  </a>
  <a href="https://github.com/domtronn/spaceline-all-the-icons.el/pulls">
    <img src="https://img.shields.io/issuestats/p/github/domtronn/spaceline-all-the-icons.el.svg" />
  </a>
  <a href="https://github.com/domtronn/spaceline-all-the-icons.el/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/mashape/apistatus.svg" />
  </a>
</p>

## Installation

You should be able to install this package in the standard way, add it
to the load path and then calling

```el
(package-install 'spaceline-all-the-icons)

(require 'spaceline-all-the-icons)
;; or
(use-package spaceline-all-the-icons)
```

To enable git changes count, please enable git-gutter-mode
```
(global-git-gutter-mode 1)
```

*N.B.* This package is *_highly_* dependent
on [`all-the-icons.el`](https://github.com/domtronn/all-the-icons.el), so make
sure you have the [fonts](https://github.com/domtronn/all-the-icons.el/tree/master/fonts)
installed correctly

## Usage

The simplest way to use this package is to set the `mode-line-format`
to be the `spaceline-all-the-icons` theme or by calling
`spaceline-all-the-icons-theme`

```el
(use-package spaceline-all-the-icons 
  :after spaceline
  :config (spaceline-all-the-icons-theme))
```

### Optional dependencies

There are a few segments that require optional packages and custom
setup functions in order to run.  Calling these two functions in your
setup/config will enable the segments.

```el
(spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
(spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
```

## Customization

The mode line is broken down into _segments_, where each segment can
be toggled on or off independently.

Also, some of the segments have _icon sets_ associated with them and
can be modified to your taste.

You can see all of these options by calling

```el
M-x customize-group spaceline-all-the-icons
```

### Custom Segments

You can add your own custom segments to the theme by installing it
using the `spaceline-all-the-icons-theme` function and passing the
segment symbols as arguments.

```el
(spaceline-all-the-icons-theme 'your-segment-symbol "Hello" 'etc)
```

These segments will appear in order on the right side of the mode
line, in the empty middle section before all other right aligned segments.

### Disabled Segments

Some segments are _disabled by default_ you can turn them on by
calling the following in your `:config`

```el
(spaceline-toggle-all-the-icons-SEGMENT-on)
```

| Segment | Description |
| :-- | :-- |
| `bookmark` | Indicates whether the current file is [Bookmarked](https://emacswiki.org/emacs/BookMarks) |
| `dedicated` | Indicates whether the current file is dedicated |
| `fullscreen` | Indicates whether the frame is Fullscreen  |
| `buffer-position` | Indicates the position through the buffer as a percentage  |
| `narrowed` | Indicates whether the current buffer has been narrowed  |

### Icon Sets

The following segments can have their icon sets customized. You can
choose these by calling the following or setting it in `:config`

```el
M-x customize-group spaceline-all-the-icons-icon-set

(setq spaceline-all-the-icons-icon-set-modified 'toggle)
```

These segments can have one of the following icon sets

| Segment | Available Icons |
| :-- | :-- |
| `modified` | `chain`, `toggle`, `circle` |
| `bookmark` | `bookmark`, `heart`, `star` |
| `dedicated` | `pin`, `sticky-note` |
| `window-numbering` | `circle`, `solid`, `string`, `square` |
| `multiple-cursors` | `caret`, `pointer` |
| `git-stats` | `diff-icons`, `arrows` |
| `flycheck-slim` | `solid`, `outline`, `dots` |
| `sun-time` | `rise/set`, `sun/moon`, `arrows` |

### Separators

You can customize the divider separators using
`spaceline-all-the-icons-separator-type`, the available types are:
`'slant`, `'arrow`, `'cup`, `'wave`, `'none`

You also have two types of separators between individual segments,
these are `spaceline-all-the-icons-primary-separator` &
`spaceline-all-the-icons-secondary-separator`. Their defualt values
are `|` & `·` respectively.

### Debugging

When a segment throws an error in Spaceline, this causes the entire
mode line to disappear _(be blank)_, this is since the
`mode-line-format` throws an error. 

Spaceline unfortunately hides a lot of errors internally and doesn't
given you information as to which segment is throwing an error.

To debug this, try running 

```el
M-x spaceline-all-the-icons--debug-segments
C-u M-x spaceline-all-the-icons--debug-segments
```

This will _(should)_ return a list of segments which are throwing errors and will
help me to debug any issues you're having!

Calling it with a `C-u` prefix will _(should)_ disable the segments
that are currently erroring so that at least the mode line will work.

[▲ back to top](#readme)
