LazPackager
===========

What is it?
-----------
LazPackager is a Lazarus plugin (a package that you can install in your IDE)
that will enable you to create an installable (optionally signed) binary
Debian package (in the future also rpm, inno, and others) from any existing
Lazarus project. (In its current early stage only Debian is supported)

You can also use it to create a so called Debian "source package" and upload
to Launchpad. A Debian Source package is a tarball with the umodified original
sources along with signed hashes and a signed diff that adds all the necessary
Debian voodoo to allow automated building and packaging on their build farm.

see here for some screenshots: 
[http://prof7bit.github.com/LazPackager/](http://prof7bit.github.com/LazPackager/)

How does it work?
-----------------
When you invoke LazPackager to make a Debian package it will create and
run a build script that will do the following: make a copy of the source
tree, create a tar.gz, add the necessary control files, rules and other
meta data and a Makefile to the copied source, invoke the debuild tool
to build it, optionally sign it with debsign (using your gpg key) and
optionally upload it with dput to your Launchpad PPA where it will be
enqueued for building automatically.

LazPackager will store some additional information about your project in
the project file (.lpi) in the CustomData section. Some needed information
can be inferred from the Lazarus project settings automatically, other
things need to be configured separately. It will also store any changes
and additional tweaks you made to the default debian templates (Makefile,
control, rules, changelog, copyright) in the project file.

For using the signing and uploading feature for Launchpad you will need
to have gpg installed and have a valid key-pair for your email address.
When uploading to Launchpad this must be the **same** email address and
gpg key you used when signing their "code of conduct" and also the same
email address that you configure in lazdebian.

LazPackager will not store any confidential information in the config file,
the only password you need to provide is the passphrase for your gpg key
and for this it will pop up a console and let gpg ask it from you
directly. You do not need your Launchpad password for uploading because
they will authenticate you and your uploaded files by your valid gpg
signature.


Does it work yet?
-----------------
95%. Should work for most simple projects out of the box and with
a few tweaks to the templates for all others too.
