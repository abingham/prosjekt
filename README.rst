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

  (require 'projeckt)
  (require 'anything-prosjekt)

  (require 'anything)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
  (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)

Now you can create a project with "M-x prosjekt-new". You will be
asked for a project name and a top-level directory. The project is
immediately opened after creation.

Now that you have an open project, you can add files to it with "M-x
prosjekt-add" which will prompt you for a file name.

Once you've got files in your project, you can access them via the
"anything" interface. Likewise, you can open projects via anything as
well.

To configure your project, you can run "M-x prosjekt-setup". This will
open a buffer allowing you to edit the emacs s-expression defining
your project. After editing, press "ESC" to save the edits. See the
sections below for more information on defining commands for your
project.

You can save the state of your project at any time with "M-x prosjekt-save".

Finally, when you done working on your project, run "M-x prosjekt-close".

.. Topics
.. ======

.. * Creating a new project
.. * Opening, saving, and closing a project
.. * Project setup
..   * Defining commands
.. * Adding files to a project
..   * Using "populate" to add many files.
..   * Setting up a "populate-spec"
.. * anything integration
.. * Files
..   * Individual projects
..   * Global project listp
.. * Command examples:
..   * running make
..   * or scons
..   * git-status
..   * Execute unittests
