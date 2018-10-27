# Contributing

1. <a href="#important-notes">Important notes</a>
2. <a href="#resources-tree">Resources tree</a>
3. <a href="#git">Git</a>
   * <a href="#key-branches">Key branches</a>
   * <a href="#commits">Commits</a>
   * <a href="#pull-requests">Pull requests</a>
   * <a href="#reporting-issues">Reporting issues</a>
4. <a href="#code">Code</a>
   * <a href="#bootstrap">Bootstrap</a>
   * <a href="#html">HTML/JADE</a>
   * <a href="#css">CSS/LESS</a>
   * <a href="#js">Javascript</a>

## <span id="important-notes">Important notes</span>
* All code in any code base should look like a single person typed it, no matter how many people contributed. This means strictly enforcing these agreed upon guidelines at all times.
* Your Grunt watcher should be always running when contributing to a project.
* Don't edit files in the `public` subdirectory as they are generated via grunt. You'll find source code in the `app` subdirectory!

## <span id="resources-tree">Resources tree</span>
Standards files for the web app are located in `app/assets/*`.
Favicons, robots file, stylesheets, javascripts, vendors etc..

## <span id="git">Git</span>
1. [Git-scm.com](http://git-scm.com/book/) - Your number one `git` reference.
2. [Git-convenience](https://github.com/jakearchibald/git-convenience) - Basic `git` shortcuts (`gc` for `git commit`, `gs` for `git status`, etc..).
3. [Git-extras](https://github.com/visionmedia/git-extras) - Advanced `git` shortcuts (`git-create-branch` to create a local and a remote branch, etc..).
4. [Git-up](https://github.com/aanand/git-up) - Branch sync with rebase.
5. [Git-wtf](http://git-wt-commit.rubyforge.org/#git-wtf)

### <span id="key-branches">Key branches</span>
* `master` is the latest, deployed version. The master branch needs to always be clean, lean and stable. **Very stable**.. If we want to show or deploy the project to someone, we show them the master branch or one of its tags.
* `gh-pages` is the hosted docs (not to be used for pull requests).
* `*-wip` is the official work in progress branch for the next release.
  * `yourname-wip` is your general development branch.
  * `featurename-wip` are feature specific branches.

### <span id="commits">Commits</span>
1. **Commits are per 1 subject.**
   As an example, if you fix a bug and along the way you made some function a little faster, then those should be 2 different commits.

2. You can use `git add -p` to interactively select which changes get staged before you commit.

3. When you then commit, you need to provide a one line short commit message and **then** a paragraph or two explaining the change you made in details.
   * There should be one blank line between the short and long description.
   * If it's a small (obvious and trivial), 1 or 2 line change, then only the short description is sufficient.
   * This makes it easier to revert a commit if it introduces a bug. It also makes it easier to merge and rebase.

4. **After every commit, the whole project must at least build and run.**

### <span id="pull-requests">Pull requests</span>
1. Create a new branch, please don't work in your `master` branch directly.
2. Fix stuff.
3. Run `grunt` to see if it still builds. Repeat steps 2-3 until done.
4. Update the documentation to reflect any changes.
5. Push to your fork and submit a pull request. (Try to submit pull requests against the latest `*-wip` branch for easier merging)

### <span id="reporting-issues">Reporting issues</span>
We only accept issues that are bug reports. Bugs must be isolated and reproducible problems that we can fix within the Application core. Please read the following guidelines before opening any issue.

1. **Search for existing issues.** We get a lot of duplicate issues, and you'd help us out a lot by first checking if someone else has reported the same issue. Moreover, the issue may have already been resolved with a fix available.
2. **Include a test case.** Supply the page name and the concerned zone.
3. **Share as much information as possible.** Include operating system and version, browser and version. Also include steps to reproduce the bug.


## <span id="code">Code</span>
* Two spaces for indentation, never tabs.

### <span id="bootstrap">Bootstrap</span>
* Do NOT rewrite the wheel. Make use Bootstrap as much as you can. [Learn it](http://twitter.github.com/bootstrap/).
* Customize it's variables in `assets/stylesheets/variables.less`.
* Never desactivate low-resolution responsive stylesheets.
* Use their grid system. [Learn it](http://twitter.github.com/bootstrap/scaffolding.html#gridSystem).
* **Never style bootstrap's grid elements such as `.span4`**
* **DO NOT remove any of bootstrap's reset styles** such as line heights and bottom margins.

### <span id="html">HTML</span>
* Use JADE.
* Double quotes only, never single quotes.
* Use tags and elements appropriate for an HTML5 doctype.
* Enforce standards mode in every browser possible with this simple doctype at the beginning of every HTML page. `<!DOCTYPE html>`
* Strive to maintain HTML standards and semantics, but don't sacrifice pragmatism. Use the least amount of markup with the fewest intricacies whenever possible.
* Writing markup in a javascript file makes the content harder to find, harder to edit, and less performant. **Don't do it.**
* Learn [JADE](https://github.com/visionmedia/jade/blob/master/Readme.md).
* **Use** available mixins in the `views/mixins` subdirectory. Add new ones.
* You must **`extend`** the `app-layout` **rather than `include`** a template in it.

#### Attribute order
HTML attributes should come in this particular order for easier reading of code.

1. class
2. id
3. data-*
4. for|type|href

Such that your markup looks like:

```
<a class="" id="" data-modal="" href="">Example link</a>
```

### <span id="css">CSS</span>
* Multiple-line approach (one property and value per line).
* Always a space after a property's colon (.e.g, `display: block;` and not `display:block;`).
* End all lines with a semi-colon.
* Attribute selectors, like `input[type="text"]` should always wrap the attribute's value in double quotes, for consistency and safety [read more](http://mathiasbynens.be/notes/unquoted-attribute-values).
* When grouping selectors, keep individual selectors to a single line
* Include one space before the opening brace of declaration blocks
* Place closing braces of declaration blocks on a new line
* Comma-separated values should include a space after each comma
* Don't include spaces after commas in RGB or RGBa colors, and don't preface values with a leading zero
* Lowercase all hex values, e.g., `#fff`instead of `#FFF`
* Use shorthand hex values where available, e.g., `#fff` instead of `#ffffff`
* Avoid specifying units for zero values, e.g., `margin: 0;` instead of `margin: 0px;`
* Learn [LESS](http://lesscss.org/) and use it.
* Use LESS' mixins, variables and Math.
* Organize sections of code by component.
* Have one `less` file per module.
  **The `no-js` CSS rules must be included inside each and every one of them (at the very bottom).**


**Incorrect example:**

```
.selector, .selector-secondary, .selector[type=text] {
  padding:15px;
  margin:0px 0px 15px;
  background-color:rgba(0, 0, 0, 0.5);
  box-shadow:0 1px 2px #CCC,inset 0 1px 0 #FFFFFF
}
```

**Correct example:**

```
.selector,
.selector-secondary,
.selector[type="text"] {
  padding: 15px;
  margin: 0 0 15px;
  background-color: rgba(0,0,0,.5);
  box-shadow: 0 1px 2px #ccc, inset 0 1px 0 #fff;
}
```

Questions on the terms used here? See the [syntax section of the Cascading Style Sheets article](http://en.wikipedia.org/wiki/Cascading_Style_Sheets#Syntax) on Wikipedia.


#### Declaration order

Related declarations should be grouped together, placing positioning and box-model properties closest to the top, followed by typographic and visual properties.

```
.declaration-order {
  /* Positioning */
  position: absolute;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  z-index: 100;

  /* Box-model */
  display: block;
  float: right;
  width: 100px;
  height: 100px;

  /* Typography */
  font: normal 13px "Helvetica Neue", sans-serif;
  line-height: 1.5
  color: #333;
  text-align: center;

  /* Visual */
  background-color: #f5f5f5;
  border: 1px solid #e5e5e5;
  border-radius: 3px;

  /* Misc */
  opacity: 1;
}
```

For a complete list of properties and their order, please see [Recess](http://twitter.github.com/recess).


#### Formatting exceptions

In some cases, it makes sense to deviate slightly from the default [syntax](#css-syntax).

##### Prefixed properties

When using vendor prefixed properties, indent each property such that the value lines up vertically for easy multi-line editing.

```
.selector {
  -webkit-border-radius: 3px;
     -moz-border-radius: 3px;
          border-radius: 3px;
}
```

In Sublime Text 2, use **Selection &rarr; Add Previous Line** (&#8963;&#8679;&uarr;) and **Selection &rarr;  Add Next Line** (&#8963;&#8679;&darr;).

##### Rules with single declarations
In instances where several rules are present with only one declaration each, consider removing new line breaks for readability and faster editing.

```
.span1 { width: 60px; }
.span2 { width: 140px; }
.span3 { width: 220px; }

.sprite {
  display: inline-block;
  width: 16px;
  height: 15px;
  background-image: url(../img/sprite.png);
}
.icon           { background-position: 0 0; }
.icon-home      { background-position: 0 -20px; }
.icon-account   { background-position: 0 -40px; }
```


#### Human readable
Code is written and maintained by people. Ensure your code is descriptive, well commented, and approachable by others. Use self explanatory names.

##### Comments
Great code comments convey context or purpose and should not just reiterate a component or class name.

**Bad example:**

```
/* Modal header */
.modal-header {
  ...
}
```

**Good example:**

```
/* Wrapping element for .modal-title and .modal-close */
.modal-header {
  ...
}
```

##### Class names

* Keep classes lowercase and use dashes (not underscores or camelCase)
* Avoid arbitrary shorthand notation
* Keep classes as short and succinct as possible
* Use meaningful names; use structural or purposeful names over presentational
* Prefix classes based on the closest parent component's base class

**Bad example:**

```
.t { ... }
.red { ... }
.header { ... }
```

**Good example:**

```
.tweet { ... }
.important { ... }
.tweet-header { ... }
```

##### Selectors

* Use classes over generic element tags
* Keep them short and limit the number of elements in each selector to three
* Scope classes to the closest parent when necessary (e.g., when not using prefixed classes)

**Bad example:**

```
span { ... }
.page-container #stream .stream-item .tweet .tweet-header .username { ... }
.avatar { ... }
```

**Good example:**

```
.avatar { ... }
.tweet-header .username { ... }
.tweet .avatar { ... }
```

### <span id="js">Javascript</span>
* Never alter the JSLint configuration in Gruntfile.js nor desactivate the JSLint task!
* Single quotes only, never double quotes unless it's for writing out HTML.
* One javascript library only, either jQuery or Zepto.
* One carousel plugin only, Flexslider.
* Code à la bootstrap way.
  * No main initialization for the application, every module auto-initializes itself depending on the presence of elements with `data-attributes` on the page.
  * Event driven to interact between different modules.
* Use `assets/javascripts/app-plugin-sample.js` to create new javascript modules. Rename it, keep the `app-` prefix.
* If you want to add an exisiting bootstrap javascript component:
  * Lint it first.
  * Add it to `assets/javascripts`.

#### Bootstrap plugins
Not all bootstrap plugins are included. If you need a new plugin, first check the [Bootstrap's plugins page](https://github.com/twitter/bootstrap/tree/master/js). If you don't find what you'r looking for, either write your own `assets/javascripts/app-plugin-sample.js`, or add a new vendor asset.

#### Carousels & Sliders
Use jQuery Flexslider as your main <u>**and only**</u> carousel plugin. It's responsive and has native support for real swipe events.
**A bootstrap middleware `assets/javascripts/app-carousels.js` has been created for this plugin.**
