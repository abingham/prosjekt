==============
IMPORTANT NOTE
==============

The original author of ``prosjekt`` is not likely to actively maintain it. He's moved over to using ``projectile``, and unless that proves to be really unsuitable (not the case so far) he'll stick with it and abandon ``prosjekt``.

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

Lineage
=======

Prosjekt was originally based on, and still has many parallels to,
the wonderful `eproject
<http://www.emacswiki.org/emacs-en/eproject>`_ emacs
package. Prosjekt aims to maintain the elegance and simplicity of
eproject while extending it and approaching some of the design
details differently.

Quickstart
==========

First install Prosjekt by copying ``prosjekt.el`` and
``helm-prosjekt.el`` to your emacs load path.

Next, require ``prosjekt`` and ``helm-prosjekt`` in your emacs
config and enable ``helm`` integration::

  (require 'prosjekt)
  (require 'helm-prosjekt)

(Note that the use of ``helm`` with prosjekt is entirely
optional. If you don't want to use it then just don't use the lines
with the string ``helm`` in them.)

Now you can create a project with ``M-x prosjekt-new``. You will be
asked for a top-level directory and an optional project name. 
Prosjekt creates a ``.prosjekt`` file for every project it manages, 
and the optional name is prepended to that filename. You typically 
only need a name if you're putting more than one project in a 
directory. The project is immediately opened after creation.

Now that you have an open project, you can add files to it with ``M-x
prosjekt-add`` which will prompt you for a file name.

Once you've got files in your project, you can access them via the
``helm`` interface. Likewise, you can open projects via ``helm`` as
well.

To configure your project, you can run ``M-x prosjekt-setup``. This
will open a buffer allowing you to edit the emacs s-expression
defining your project. After editing, press ``ESC`` to save the edits
(or use ``CTRL-ESC`` to save without killing the buffer.) See the
sections below for more information on defining commands for your
project.

You can save the state of your project at any time with ``M-x
prosjekt-save``. (Though for the most part this is done for you
automatically.)

Finally, when you done working on your project, run ``M-x
prosjekt-close``.

Basics
======

A project in Prosjekt is essentially:

 * A name
 * A directory representing the top level of your project
 * A collection of files in your project
 * A set of commands that can be run on your project

To work on a project, you must first open it with ``prosjekt-open``
(or via the ``helm`` interface, generally with ``C-u``). Opening a
project closes any other project you had open, and makes the files in
the project available through the ``helm`` interface. Likewise, the
commands for a project are only active when a project is open.

As you work with a project, you might edit it in various ways, for
example:

 * Add or remove files
 * Add, remove, or edit the commands
 * Modify the project's ``include`` and ``ignore`` lists

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
command. This will ask you for the file containing a project
description. If you're using prosjekt's ``helm`` integration, you can
open projects via ``helm``; ``helm`` is generally bound to ``C-u``.

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
no longer be accessible through prosjekt's ``helm`` integration.

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

Project commands are emacs functions that you can execute from within
your project with project-specific keybindings. Each project can have
any number of commands, each with its own keybindings.

You can configure your commands with the ``prosjekt-setup``
command. The commands are all under the "tools" key in the project
expression. For example, commands in ``prosjekt-setup`` might look
something like this::

  (...
   (:tools
    ((:keys "[f5]")
     (:command . git-status)
     (:name . "git status"))
    ((:keys "[f6]")
     (:command compile "scons -j12")
     (:name . "compile"))
    ((:keys "[f7]")
     (:command gdb "gdb --annotate=3 my_program")
     (:name . "run gdb"))
    ((:keys "[f8]")
     (:command shell-command "ctags -f TAGS -e -R .")
     (:name . "ctags"))
   ...
  )

This defines four command. The first binds the interactive emacs
function ``git-status`` to the key ``f5``. The second bind a scons
compilation command to ``f6``. The third binds ``f7`` to the
non-interactive emacs function invocation for launching gdb on a
particular program and assigns it the name "run gdb". The fourth binds
``f8`` to a shell command for rebuilding a ctags index.

More generally, each command definition is an alist of ``((:keys
. . .)  (:command . . .) (:name . "name"))``. The keybindings must be
strings suitable as the second argument to the standard ``define-key``
function. The command type must be an emacs command that can be called
with zero arguments. The name can be used to invoke the command by
name with the ``prosjekt-run-tool-by-name`` function.

Command examples
----------------

Here are a few example commands that you might find useful. The first
executes ``make`` from the root of the project in a compilation buffer
when ``f5`` is pressed::

  ((:keys "[f5]")
   (:command compile "make")
   (:name "compile"))

This next one runs the ``ahg-status`` emacs function (for querying the
status of a mercurial repository) when ``control-shift-f7`` is
pressed::

  ((:keys "[C-S-f7]")
   (:command . ahg-status)
   (:name . "hg"))

This example first switches to a new directory and then executes a
test suite. Note that this assumes bash-like syntax::

  ((:keys "[C-f6]")
   (:command shell-command "cd tests && ./test_suite")
   (:name . "tests"))

This final example is an interesting and powerful tool. It prompts the
user for a command to run and executes that command at the project
root::

  ((:keys "[f9]")
   (:command . shell-command)
   (:name . "shell command"))

In your ``prosjekt-setup`` buffer these might look like this::

  ((:name . name)
   (:tools
    ((:keys "[f5]")
     (:command compile "make")
     (:name "compile"))
    ((:keys "[C-S-f7]")
     (:command . ahg-status)
     (:name . "hg"))
    ((:keys "[C-f6]")
     (:command shell-command "cd tests && ./test_suite")
     (:name . "tests"))
    ((:keys "[f9]")
     (:command . shell-command)
     (:name . "shell command")))
   (:include "\\.py$" "\\.rst$")
   (:ignore ".*~")
  )

Hooks
=====

Prosjekt supports a number of hooks that you can use to run functions
at specific times. To add your own hook functions, simply put them on
the appropriate hook list::

  (add-to-list 'prosjekt-hook-name 'my_hook_function)

``prosjekt-open-hooks``
-----------------------

The ``prosjekt-open-hooks`` are run whenever *any* project is
opened. The hooks are run after the project is fully opened, i.e. at
the end of the open logic.

``prosjekt-close-hooks``
------------------------

The ``prosjekt-close-hooks`` are run whenever *any* project is
closed. The hooks are run before any other processing takes places,
i.e. at the start of the close logic.

Embedded hooks
--------------

You can also embed project-specific hooks in a project configuration
with the ``open-hooks`` and ``close-hooks`` entries. These hooks are
defined entirely in your project configuration (though they can, of
course, call other functions), and unlike the global hooks they are
only executed for the project in which they're defined.

For example, you can define a project-specific open-hook in a project
configuration like this::

  (...
   (:open-hooks
    (lambda () (message "my embedded open hook")))
   ...
  )

The various embedded hooks are executed immediately after their
corresponding global hooks, i.e. the embedded "open-hooks" are run
right after the ``prosjekt-open-hooks``.

Project population
==================

While you can add files to your projects via the ``prosjekt-add``
command, this can be tedious for larger projects. To address this,
Prosjekt supports the notion of "populating" a project. This
essentially means finding all of the files under a directory and
adding those files to you project.

The first command for project population is
``prosjekt-populate``. This asks you for a directory and a list of
regular expressions, looking for files under that directory which do
not match any of the regular expressions, recursively, and adding the
matches to your project. You invoke it like this::

    (prosjekt-populate "/my/project" '(".*~"))

The regular expressions should be suitable as the first argument to
the ``string-match`` function.

``includes``, ``ignores``, and ``prosjekt-repopulate``
------------------------------------------------------

Another way to populate your project is by defining an ``:ignores``
and ``:includes`` list in your project config and then running
``prosjekt-repopulate``.  Both are in your project configuration
assoc-list, the ``cdr`` of which are lists of regular expressions.

The ``prosjekt-repopulate`` first clears the project's file list. It
then simply scans each specified directory for files that match an
entry in the ``includes``. Any of these matches which doesn't *also*
match an entry in ``ignores`` is added to the project.

For example, to ignore all ``.pyc`` and ``.so`` files under the
project root you would set your ``:includes`` and ``:ignores`` like
this::

  (...
   (:includes ".*")
   (:ignores "\\.pyc$" "\\.so$")
  )

``prosjekt-repopulate`` was initially designed for new projects under
heavy development where the contents of a project can change quickly,
and it's very useful for keeping a project definition up to date with
changes coming from other developers.

helm integration
================

Prosjekt can integrate with the brilliant `helm
<http://emacswiki.org/emacs/Helm>`_ package via
``helm-prosjekt.el``. Generally all you need to do to enable ``helm``
integration is to load ``helm-prosjekt.el``::

  (require 'helm-projekt)

This adds two sources to ``helm``. The first is your list of Prosjekt
projects by name. You can open a Prosjekt project just by specifying
it to ``helm``.

The second source is the list of files in your current project (if
any.) As with projects, you can open project files just by invoking
``helm``.

Files used by prosjekt
======================

Prosjekt uses two types of files to keep track of your various
projects. The first is the global configuration file, "<home
directory>/.emacs.d/prosjekt.lst". This is primarily just a list of
your projects definition files. There is only one global configuration
file.

The second type of file used by prosjekt is a project
description. Each of your projects has its own project description,
and the file is named "<project root directory>/<project name>.prosjekt". This
file contains the list of files in a project, the command definitions
for the project, the project's populate spec, and various other bits
of information.
