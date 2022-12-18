Unbound Bible
===========

The Unbound Bible is an open source and a free, multilingual Bible-reader program for Windows, macOS and Linux. This is fast and effective way to explore bibles.


Build Instructions
------------------

(Tested under Linux.)

Install ZeosDBO (see the [Lazarus wiki](https://wiki.freepascal.org/ZeosDBO) for full instructions):

```bash
$ cd components
$ svn co http://svn.code.sf.net/p/zeoslib/code-0/trunk
$ mv trunk zeosdb
$ cd ..
```

Compile the dependent packages:

```bash
$ lazbuild components/zeosdb/packages/lazarus/zcomponent.lpk
$ lazbuild components/richmemo/richmemopackage.lpk
$ lazbuild components/unboundmemo/unboundmemopackage.lpk
```

Build unboundbible:

```bash
$ lazbuild unboundbible.lpr
```

The binary will be in the current folder.


