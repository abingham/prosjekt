=============================================
 Prosjekt: A software project tool for emacs
=============================================

Prosjekt is an emacs package for managing software projects. A project
in Prosjekt is defined as:

 * A top-level project directory
 * Files under that directory which are part of the project
 * Commands which can be executed on the project

The files in a project are generally just the source files for your
software: headers, implementation, resources, makefiles, etc. Prosjekt
places no limitations on what you can associate with a project. The
commands are generally things like compilation, test execution, and
source control interaction. Prosjekt currently supports executing both
shell commands and emacs functions as commands, leveraging emacs'
existing capabilities as much as possible.

.. admonition:: Lineage

  Prosjekt was originally based on, and still has many parallels to,
  the wonderful `eproject
  <http://www.emacswiki.org/emacs-en/eproject>`_ emacs
  package. Prosjekt aims to maintain the elegance and simplicity of
  eproject while extending it and approaching some of the design
  details differently.

Quickstart
==========

First install Prosjekt by copying "prosjekt.el" and
"anything-prosjekt.el" to your emacs load path.

Next, require "prosjekt" and "anything-prosjekt" in your emacs
config and enable anything integration::

  (require 'prosjekt)
  (require 'anything-prosjekt)

  (require 'anything)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)

(Note that the use of "anything" with prosjekt is entirely
optional. If you don't want to use it then just don't use the lines
with the string "anything" in them.)

Now you can create a project with ``M-x prosjekt-new``. You will be
asked for a project name and a top-level directory. The project is
immediately opened after creation.

Now that you have an open project, you can add files to it with ``M-x
prosjekt-add`` which will prompt you for a file name.

Once you've got files in your project, you can access them via the
"anything" interface. Likewise, you can open projects via anything as
well.

To configure your project, you can run ``M-x prosjekt-setup``. This
will open a buffer allowing you to edit the emacs s-expression
defining your project. After editing, press ``ESC`` to save the edits
(or use ``CTRL-ESC`` to save without killing the buffer.) See the
sections below for more information on defining commands for your
project.

You can save the state of your project at any time with ``M-x prosjekt-save``.

Finally, when you done working on your project, run ``M-x prosjekt-close``.

Basics
======

A project in Prosjekt is essentially:

 * A name
 * A directory representing the top level of your project
 * A collection of files in your project
 * A set of commands that can be run on your project

To work on a project, you must first open it with ``prosjekt-open``
(or via the anything interface, generally with ``C-u``). Opening a
project closes any other project you had open, and makes the files in
the project available through the anything interface. Likewise, the
commands for a project are only active when a project is open.

As you work with a project, you might edit it in various ways, for
example:

 * Add or remove files
 * Add, remove, or edit the commands
 * Modify the project's populate-spec

At any time you can save the state of a project with
``prosjekt-save``.

Finally, when you're done working on a project, use ``prosjekt-close``
to close it.

Creating a new project: ``prosjekt-new``
----------------------------------------

You create a new project with the ``prosjekt-new`` command. This will
ask you for a project name and a root directory for the project. This
command will create a new project file you and update the master
project list. New projects are immediately opened, closing any other
projects you currently have open.

Opening an existing project: ``prosjekt-open``
----------------------------------------------

You can open an existing project with the ``prosjekt-open``
command. This will ask you for the name of the project. If you're
using prosjekt's anything integration, you can open projects via
anything; anything is generally bound to ``C-u``.

Opening a project will save and close any other projects you have open.

Saving a project: ``prosjekt-save``
-----------------------------------

At any time you can save the contents of your current project with
``project-save``. This writes the current state of your project to the
project file.

Closing a project: ``prosjekt-close``
-------------------------------------

You can close a project with ``prosjekt-close``. This will first save
the contents of your project, and then close it. This deactivates your
projects command and key bindings, and the files in your project will
no longer be accessible through prosjekt's anything integration.

Configuring a project: ``prosjekt-setup``
-----------------------------------------

The state of a project is represented with a single emacs-lisp
assoc-list. Each entry in the list represents some element of the
project definition: name, files, commands, etc. You can edit this
expression directly with the ``prosjekt-setup`` command.

When you execute this command, the configuration expression will be
brought up in an editable buffer. You can then edit the expression as
you like. You can press ``ESC`` to save the configuration and kill the
buffer. Or, you can use ``CTRL-ESC`` to save the configuration without
killing the buffer.

Note that the ``prosjekt-setup`` buffer initially displays a
pretty-printed version of the emacs expression defining your
project. You are free to stray from the particular formatting of this
buffer *as long as you keep a valid and structurally correct
expression*. That is, the exact spacing and indentation of the buffer
contents don't matter as long as the contents parse as a proper emacs
list expression. Also note that any formatting you apply will not be
remembered; the buffer contents are recreated each time you execute ``prosjekt-setup``.

Project commands
================

Project commands are shell commands or emacs functions that you can
execute from within your project with project-specific
keybindings. Each project can have any number of commands, each with
its own keybindings.

You can configure your commands with the ``prosjekt-setup``
command. The commands are all under the "tools" key in the project
expression. For example, commands in ``prosjekt-setup`` might look
something like this::

  (...
   ("tools"
    ("[f5]" "interactive" git-status)
    ("[f6]" "shell" "scons -j12")
    ("[C-f6]" "shell" "scons -u")
    ("[f7]" "call" (gdb "gdb --annotate=3 my_program"))
   ...
  )

This defines four command. The first binds the interactive emacs
function ``git-status`` to the key ``f5``. The second binds the ``scons
-j12`` shell command to ``f6``. The third binds ``scons -c`` to
``control-f6``. The fourth binds ``f7`` to the non-interactive emacs
function invocation for launching gdb on a particular program.

More generally, each command definition is a list of ``(key-binding
type command)``. The keybinding must be a string suitable as the
second argument to the standard ``define-key`` function. The command
type must be one of:

 * *interactive* - This calls an interactive emacs function. The
    ``command`` argument must be the name of the interactive function.
 * *call* - This calls a non-interactive emacs function. The
    ``command`` argument must be a list of the function and all of its
    arguments.
 * *shell* - This calls a shell command from the root directory of the
    project. The ``command`` argument must be a string specifying the
    full command to execute. The command will be run in an emacs
    compilation buffer.

Command examples
----------------

Here are a few example commands that you might find useful. The first
executes ``make`` from the root of the project when ``f5`` is
pressed::

  ("[f5]" "shell" "make")

This next one runs the ``ahg-status`` emacs function (for querying the
status of a mercurial repository) when ``control-shift-f7`` is
pressed::

  ("[C-S-f7]" "interactive" ahg-status)

This last example first switches to a new directory and then executes
a test suite. Note that this assumes bash-like syntax::

  ("[C-f6]" "shell" "cd tests && ./test_suite")

In your ``prosjekt-setup`` buffer these might look like this::

  (("name" . name)
   ("tools"
    ("[f5]" "shell" "make")
    ("[C-S-f7]" "interactive" ahg-status)
    ("[C-f6]" "shell" "cd tests && ./test_suite"))
   ("files
    (..etc...)
   ))

Project population
==================

While you can add files to your projects via the ``prosjekt-add``
command, this can be tedious for larger projects. To address this,
Prosjekt supports the notion of "populating" a project. This
essentially means finding all of the files under a directory that
match a particular regular expression, and adding those files to you
project.

The first command for project population is
``prosjekt-populate``. This asks you for a directory and a regular
expression, looking for files under that directory which match the
regular expression, recursively, and adding the matches to you
project. You invoke it like this::

  M-x prosjekt-populate <RET> "/my/project" <RET> "\.cpp$" <RET>

The directory argument will default to your project's root
directory. The regular expression should be suitable as the first
argument to the ``string-match`` function.

So, for example, to add all of the ``.py`` files under your project's
``src`` directory, you would execute::

  (prosjekt-populate "/my/project/src" "\.py$")

``populate-spec`` and ``prosjekt-repopulate``
---------------------------------------------

Another way to populate your project is by defining a "populate-spec"
in your project config and then running ``prosjekt-repopulate``.
``populate-spec`` is an optional entry in your project configuration
assoc-list, the ``cdr`` of which is a list of elements of the form
``(project-relative-directory regex1 regex2 . . .)``.

The ``prosjekt-repopulate`` first clears the project's file list. It
then simply scans each specified directory for files matching any of
the regular expressions, adding each match to the project's file list.

For example, to specify the following project contents::

 * All .cpp, .hpp, and .py files under ``<project-root>/src``
 * All .py files under ``<project-root>/site_scons/build_tools``

you could use a ``populate-spec`` like this::

  (...
   ("populate-spec"
    ("src" ".hpp$" ".cpp$" ".py$")
    ("site_scons/build_tools" ".py$"))
  )

``prosjekt-repopulate`` was initially designed for new projects under
heavy development where the contents of a project can change quickly,
and it's very useful for keeping a project definition up to date with
changes coming from other developers.

anything integration
====================

Prosjekt can integrate with the brilliant `anything
<http://emacswiki.org/emacs/Anything>`_ package via
anything-prosjekt.el. Generally all you need to do to enable anything
integration is to load anything-prosjekt.el::

  (require 'anything-projekt)

and then add the prosjekt "sources" to anything::

  (require 'anything)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)

This adds two sources to anything. The first is your list of Prosjekt
projects by name. You can open a Prosjekt project just by specifying
it to anything.

The second source is the list of files in your current project (if
any.) As with projects, you can open project files just by invoking anything.

Files used by prosjekt
======================

Prosjekt uses two types of files to keep track of your various
projects. The first is the global configuration file, "<home
directory>/.emacs.d/prosjekt.lst". This is nothing more than a list of
your projects along with the paths to their individual project
descriptions. There is only one global configuration file.

The second type of file used by prosjekt is a project
description. Each of your projects has its own project description,
and the file is named "<project root directory>/prosjekt.cfg". This
file contains the list of files in a project, the command definitions
for the project, the project's populate spec, and various other bits
of information.
