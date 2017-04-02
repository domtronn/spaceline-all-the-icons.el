<p align="center">
<img src="logo.png" width="1200">
</p>

<p align="center">
<b><a href="#installation">Installation</a></b>
|
<b><a href="#usage">Usage</a></b>
|
<b><a href="#customization">Customization</a></b>
</p>

## Installation

You should be able to install this package in the standard way, add it
to the load path and then calling

```el
(package-install 'spaceline-all-the-icons)

(require 'spaceline-all-the-icons-config)
;; or
(use-package spaceline-all-the-icons-config)
```

*N.B.* This package is *_highly_* dependent
on [`all-the-icons.el`](github.com/domtronn/all-the-icons.el), so make
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

## Customization

The mode line is broken down into _segments_, where each segment can
be toggled on or off independently.

Also, some of the segments have _icon sets_ associated with them and
can be modified to your taste.

You can see all of these options by calling

```el
M-x customize-group spaceline-all-the-icons
```

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
| `git-stats` | `diff-icons`, `arrows` |
| `flycheck-slim` | `solid`, `outline`, `dots` |

### Separators

You can customize the divider separators using
`spaceline-all-the-icons-separators-type`, the available types are:
`'slant`, `'arrow`, `'cup`, `'wave`, `'none`


[▲ back to top](#readme)
