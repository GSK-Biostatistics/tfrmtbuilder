# tfrmtbuilder 0.0.6

* New features:
  - Add support for `page_plan`
  - Add support for additional output formats: `rtf`, `docx`, `pdf`
  - Add support for `.xpt` data files
  - Add ability to expand and collapse table preview on "Edit" tab
  - Addition of JSON examples on the "Load" tab
  
  
* Bug fixes:
  - Fix navbar link underline css conflict to support bslib >= 0.6.0
  - Fix issue where buttons on "Load" tab overlapped on smaller screens
  - Fix issue where sorting cols selections did not trigger activation/glow of save/refresh buttons properly

# tfrmtbuilder 0.0.5

- Improve robustness of how `tfrmt` internal functions are called.


# tfrmtbuilder 0.0.4

First release to CRAN

# tfrmtbuilder 0.0.3

* Bug fixes:
  - Allow param-level formatting in `body_plan()`
  - Fix issue where user is unable to select `frmt` buttons in `body_plan()` via installed package
  - fix navbar warnings about extra content

# tfrmtbuilder 0.0.2

* Data Mapping improvements:
  - Persistent mapping selections when additional inputs are added
  - Informing user when the "save" button has yet to be pressed
* Mock mode toggle in nav bar rather than load tab


# tfrmtbuilder 0.0.1

First release
