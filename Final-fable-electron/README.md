Fable Electron Quick Start
==========================

*Modified 8/1/2017 by T. J. W. Clarke*

This demo project should be viwed *after* you have looked at 

This directory contains fully working code, so it is easy to get started.

It shows how to do the following:

* Use Electron to turn a FABLE project into a desktop app

* Use gulp to automate project build

* Use CodeMirror editor component in an app

* Use FSharp to write syntax-highlight code that integrates with Codemirror editor

* Access native files and dialogs via Electron from App code

Downloading the project
=======================

Download and unzip the project into any directory, say `fable-foo`.

Installing the project
======================

Make sure that you are in the `fable-foo` folder. *All* of the
following commands assume that you are at the root of the project: they will
not work if you are in a sub-folder!

Make sure that you have [`node`](https://nodejs.org/) v6.9 and
[`yarn`](https://yarnpkg.com/) installed. 

You can get [`yarn`](https://yarnpkg.com/) from
[its website](https://yarnpkg.com/en/docs/install), or alternatively you can
use the following command:

```
npm install --global yarn
```

**Note:** Fable works with either `npm` or `yarn`. This repository uses `yarn`
because it has [several advantages over `npm`](https://yarnpkg.com/)

----

Now you must use [`yarn install`](https://yarnpkg.com/en/docs/cli/install),
which will download all of the necessary dependencies for the project.

After the dependencies are downloaded, it will then automatically compile the
project.

**Note:** you can instead use [`yarn`](https://yarnpkg.com/en/docs/cli/install),
which is exactly the same as [`yarn install`](https://yarnpkg.com/en/docs/cli/install)

You do not need to install [Fable](http://fable.io/) globally:
[Fable](http://fable.io/) will be installed locally inside of the
`node_modules` folder under `fable-foo`. This makes it easier for other people to contribute to
your project because they do not need to install [Fable](http://fable.io/)
globally, and it also guarantees that everybody is compiling your project with
the correct version of [Fable](http://fable.io/).

The following packages are installed in the provided project by yarn: `yarn add xxx` will install xxx.

* **FABLE**. The F# to Javascript compiler is itself published as a node package. FABLE itself is written in Javascript and is cross-platform and so should work on any system however best support is provided for Windows and you are on your own getting stuff to work on linux or OS-X.
* [**electron**](http://electron.atom.io/). Electron will run Javascript in any HTML file as a desktop application, with full access to desktop APIs (files etc). Think of it is being a cut-down browser for local code. Electron allows full-featured desktop cross-platform applications to be developed using Javascript & HTML, in other words any of the technologies available to client-side web pages.
* [**codemirror**](https://codemirror.net/). CodeMirror is a Javascript programmer's editor component. This replaces a textbox in an HTML GUI allowing sophisticated editing features (see [demos](https://codemirror.net/)) including syntax highlighting. CodeMirror is designed to be easy to use as a standalone Javascript component.
* [**electron-connect**](https://www.npmjs.com/package/electron-connect). This is one of many packages that allow live reloading of electron browser windows when the underlying Javascript files change. 
* **gulp**. This is a large node utility here used minimally and optionally to watch changed files and reload the electron application `live reload` when they change.
* gulp. Gulp can optionally be used to automate live reload of the app when its files chnage (similar to yarn watch).

Adding extra packages (for example javascript widgets, or FABLE libraries) can be done at any time with `yarn add`.



----

Now that the project is successfully downloaded and installed, you need to
customize it so that it becomes **your** project:

* You will need to change the `ProjectGuid` in the `Main.fsproj` and
  `test/Test.fsproj` files.

  You cannot use the existing GUIDs. You can create a new GUID by using
  [this site](https://guidgenerator.com/).

  Every `.fsproj` file must have a different GUID. The GUID must be upper-case,
  and it must be wrapped in `{}`

* You will need to change these properties in the
  [`package.json`](https://yarnpkg.com/en/docs/package-json) file:

  * [`name`](https://yarnpkg.com/en/docs/package-json#toc-name)

  * [`description`](https://yarnpkg.com/en/docs/package-json#toc-description)

  * [`version`](https://yarnpkg.com/en/docs/package-json#toc-version)

  * [`repository`](https://yarnpkg.com/en/docs/package-json#toc-repository)

  You can use whatever name you want for your project, as long as nobody else
  has taken the name first.

  It is strongly recommended to add the following properties to the
  [`package.json`](https://yarnpkg.com/en/docs/package-json) file:

  * [`license`](https://yarnpkg.com/en/docs/package-json#toc-license)

* You will need to change `README.md`

Now your project is ready!

### Development Workflow

`yarn run watch`, with the skeleton FABLE project, will automatically (nearly instantaneously) compile changes in any F# project file into javascript. 

Optionally, if you add the code below to your HTML file, `gulp --watch` will restart the code running under electron whenever one of the files changes. If you run this in a new cmd window you have auto-compile and auto-restart whenever an F# file is saved.

How to compile your project
===========================

Your project is automatically compiled when you use
[`yarn`](https://yarnpkg.com/en/docs/cli/install)

You can instead use [`yarn run watch`](https://yarnpkg.com/en/docs/cli/run),
which will compile your project (just like
[`yarn`](https://yarnpkg.com/en/docs/cli/install)), but it will also
automatically recompile your project if you make any changes to your project's
files.

If you want to stop watch mode, just hit the `Enter` or `Return` key.

Watch mode is very convenient, and it's also **much** faster, because it only
recompiles the files that actually changed, rather than recompiling your
entire project from scratch.

After making a minor change to a single `.fs` file,
[`yarn`](https://yarnpkg.com/en/docs/cli/install) takes 4,000
milliseconds to recompile the project, but
[`yarn run watch`](https://yarnpkg.com/en/docs/cli/run) takes only 40
milliseconds! The times may be slower or faster depending on your computer,
but watch mode is always faster than a full compile.

How to run your project's unit tests
====================================

You can use [`yarn test`](https://yarnpkg.com/en/docs/cli/test) which will
compile your project and then run all of your project's unit tests.

Alternatively, you can use [`yarn test -- --watch`](https://yarnpkg.com/en/docs/cli/test)
which is the same as [`yarn test`](https://yarnpkg.com/en/docs/cli/test) except
that it automatically reruns the unit tests whenever you make any changes to your
project's files. This is much faster than using
[`yarn test`](https://yarnpkg.com/en/docs/cli/test)

You should frequently use [`yarn upgrade`](https://yarnpkg.com/en/docs/cli/upgrade)
before running your unit tests, to ensure that your code works with the latest
versions of your project's dependencies.

How to run your project as an application
=========================================

After compiling, the final JavaScript code will be in the `dist/umd/Main.js`
file.

* If you want to run it in a browser, you can open the `dist/index.html` file
  in any browser. You will need to open your browser's web console in order to
  see the output: (left-click->inspect->console)

  It uses a standard HTML `<script>` tag to load the `dist/umd/Main.js` file:

  ```
  <script src="umd/Main.js"></script>
  ```

* If you want to run it in as server-side code under [Node.js](https://nodejs.org/), you can use
  `node .` or `node dist/umd/Main.js` (they both do the same thing). This will
  however not give you a GUI, although text output will be fine.

* If you want to run your code as a desktop application with full access to the host
  file system and resources, you can use *electron*. `electron . This is effectively the same as
  a browser but with added features. See next section.

How to run your project as an application under Electron
========================================================


`yarn --watch`, with the skeleton FABLE project, will automatically (nearly instantaneously) compile changes in an F# file into javascript. `gulp --watch` will restart the code under electron whenever one of the files changes.


How to make changes to your project
===================================

You can modify the `.fs` files in the `src/fs` folder, and you can modify the
`.js` files in the `src/js` folder.

The `.fs` files are F# code, which will be compiled by [Fable](http://fable.io/).

The `.js` files are JavaScript code, which will be compiled by [Babel](http://babeljs.io/).
You can use any [ECMAScript 2015 features](https://github.com/lukehoban/es6features#readme)
which are [supported by Babel](http://babeljs.io/docs/learn-es2015/).

File `ElectronMain.js` in the top-level folder is an Electron `main` process that runs server-side Electron code and starts up the client-side (FSharp) code. You do not normally need to change it.


If you want to add more `.fs` files, you will need to edit the
`Main.fsproj` file.

As an example, if you want to add in a new `src/fs/Foo.fs` file, you will need
to add the following code to `Main.fsproj`:

```
<Compile Include="src/fs/Foo.fs" />
```

This should be placed in the same `ItemGroup` as the other `.fs` files.

Also, the order matters! If a file `Foo.fs` uses a file `Bar.fs`, then `Bar.fs`
must be on top of `Foo.fs`

That also means that `Main.fs` must be at the bottom, because it depends on
everything else.



How to import JavaScript code into F#
=====================================

First, make sure that you have the following code in your `Main.fsproj`
file:

```
<Reference Include="./node_modules/fable-core/Fable.Core.dll" />
```

Don't worry: this repository already includes the above code in
`Main.fsproj`

Now you can import `.js` code into F# by using
`Fable.Core.JsInterop.importMember`:

```
let foo = Fable.Core.JsInterop.importMember<string> "../js/foo.js"
```

**Note:** The variable must be the same in F# and JavaScript (in the above
example, the variable must be called `foo` in both F# and JavaScript)

**Note:** If the JavaScript code uses `export default` then you might need to
use `importDefault` rather than `importMember`

**Note:** You have to specify the F# type of the variable. If you don't want
to specify a type, you can instead use `obj`, which means "any type":

```
let foo = Fable.Core.JsInterop.importMember<obj> "../js/foo.js"
```

----

If you are using a lot of imports you can do this:

```
open Fable.Core.JsInterop
```

Now you no longer need the `Fable.Core.JsInterop` prefix when importing:

```
let foo = importMember<string> "../js/foo.js"
```

You can see an example in the `src/fs/Message.fs` file.

----

By convention, JavaScript files are placed into `src/js`, but you can import
JavaScript files in any folder.

You can also import builtin [Node.js](https://nodejs.org/) modules or
[npm](https://www.npmjs.com/) packages which have been downloaded with
[`yarn`](https://yarnpkg.com/en/docs/cli/install):

```
let EOL = importMember<string> "os"
```

```
let foo = importMember<string> "some-library/foo.js"
```

If the file path starts with `.` or `..` then it is relative to the `.fs` file
which contains the `importMember`

If the file path does not start with `.` or `..` then it is either a builtin
[Node.js](https://nodejs.org/) module (e.g.
[`path`](https://nodejs.org/dist/latest-v6.x/docs/api/path.html),
[`os`](https://nodejs.org/dist/latest-v6.x/docs/api/os.html), etc.) or it is
a dependency which is listed in the
[`package.json`](https://yarnpkg.com/en/docs/package-json) file.

How to upgrade your dependencies
================================

You can use [`yarn outdated`](https://yarnpkg.com/en/docs/cli/outdated) which
will tell you which of your project's dependencies are out of date.

You can then do one of the following:

* Use [`yarn upgrade foo`](https://yarnpkg.com/en/docs/cli/upgrade) which will
  upgrade the package `foo` to the latest version, and it will also
  automatically edit [`package.json`](https://yarnpkg.com/en/docs/package-json)
  to use the latest version for `foo`

* Use [`yarn upgrade`](https://yarnpkg.com/en/docs/cli/upgrade) which will
  upgrade *all* of your dependencies to the latest versions, and also edits
  [`package.json`](https://yarnpkg.com/en/docs/package-json) to use the latest
  versions.

* Manually edit [`package.json`](https://yarnpkg.com/en/docs/package-json) to
  use the latest versions, and then use
  [`yarn`](https://yarnpkg.com/en/docs/cli/install)

How to remove a dependency
==========================

You can do one of the following:

* Use [`yarn remove foo`](https://yarnpkg.com/en/docs/cli/remove) which will
  remove the package `foo`, and it will also automatically remove `foo` from
  the [`package.json`](https://yarnpkg.com/en/docs/package-json) file.

* Manually edit [`package.json`](https://yarnpkg.com/en/docs/package-json) to
  remove the `foo` package, and then use
  [`yarn`](https://yarnpkg.com/en/docs/cli/install)

Locking down your dependencies
==============================

Whenever you use [`yarn`](https://yarnpkg.com/en/docs/cli/install),
[`yarn add`](https://yarnpkg.com/en/docs/cli/add),
[`yarn upgrade`](https://yarnpkg.com/en/docs/cli/upgrade), or
[`yarn remove`](https://yarnpkg.com/en/docs/cli/remove), it will create a
[`yarn.lock`](https://yarnpkg.com/en/docs/yarn-lock) file which specifies all
of the dependencies that your project depends on (including sub-dependencies,
sub-sub-dependencies, etc.), and it also specifies the exact version for every
dependency.

When using [`yarn`](https://yarnpkg.com/en/docs/cli/install), if a
[`yarn.lock`](https://yarnpkg.com/en/docs/yarn-lock)
file exists, then it will use the exact versions which are specified in the
[`yarn.lock`](https://yarnpkg.com/en/docs/yarn-lock) file.

You should add the [`yarn.lock`](https://yarnpkg.com/en/docs/yarn-lock) file
into Git, because then everybody who contributes to your project is guaranteed
to use the exact same versions as you, which helps to prevent bugs.

You should very frequently use [`yarn outdated`](https://yarnpkg.com/en/docs/cli/outdated)
and [`yarn upgrade`](https://yarnpkg.com/en/docs/cli/upgrade) to ensure that
your dependencies are up to date.

After making any changes (such as [`yarn add`](https://yarnpkg.com/en/docs/cli/add),
[`yarn upgrade`](https://yarnpkg.com/en/docs/cli/upgrade), or
[`yarn remove`](https://yarnpkg.com/en/docs/cli/remove)) you should add
the new [`yarn.lock`](https://yarnpkg.com/en/docs/yarn-lock) into Git.
