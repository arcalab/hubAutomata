Local installation
******************

VirtuosoNext toolset is developed in Scala, and uses ScalaJS to generate JavaScript.
The toolset is developed as a sub-module of `ReoLive <https://github.com/ReoLanguage/ReoLive>`_,
and as such it requires ReoLive to run.

In addition it uses ``verifyta`` a command line tool from the `Uppaal <uppaal.org>`_ Real-Time Model Checker to
verify properties of Timed Hub Automata.

Before installing ReoLive and VirtuosoNext toolset see the full list of requirements below.

Requirements
============

* Scala building tools (`SBT <https://www.scala-sbt.org>`_)
* `Uppaal <uppaal.org>`_ Real-Time Model Checker (optional)
* Java Runtime Environment (`JRE <https://www.java.com/en/download/>`_)


Installation steps
==================

Clone the `ReoLive repository <https://github.com/ReoLanguage/ReoLive>`_


.. prompt:: bash

    git clone git@github.com:ReoLanguage/ReoLive.git
    cd ReoLive


Pull the git submodules (which will include VirtuosoNext):


.. prompt:: bash

    git submodule update --init


Use your favourite editor to edit the path to Uppaal executables in the ``global.properties`` configuration file.
For example, in Mac OS the typical path would be:

.. code:: text

    ...
    verifytaCmd = /Applications/uppaal/bin-Darwin/verifyta
    ...

Run the compilation script:

.. prompt:: bash

    ./compile.sh


Running the framework
=====================

After running the script ``compile.sh`` you can already access to the *lightweight* version of VirtuosoNext
by opening

.. prompt:: bash

    open site/hubs.html

and accessing the *LW Hubs* tab.

This lightweight version is a single javascript file autogenerated during compilation,
and gives access to most of the tools.

However, if you want to access the verification tool for Timed Hubs Automata,
it requires access to your Uppaal installation to run the models in Uppaal.

For this you need to run the server and access the `VirtuosoNext` tab, as follows.

Start the server using ``sbt``

.. prompt:: bash

    sbt server/run


Open the VirtusoNext toolset in a browser

.. prompt:: bash

    open http://localhost:9000/hubs
