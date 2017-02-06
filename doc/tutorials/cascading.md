## Order of Namespaces ##

This tutorial assumes that you know what [namespaces](/doc/tutorials/namespaces.md)
are. We will talk about [cascading lookup](/doc/help/elektra-cascading.md) here.

When Elektra looks up a key, it searches namespaces in the following order:

 * [spec](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#spec) (contains metadata, e.g. to modify elektra lookup behaviour)
 * [proc](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#proc) (process-related information)
 * [dir](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#dir) (directory-related information, e.g. `.git` or `.htaccess`)
 * [user](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#user) (user configuration)
 * [system](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#system) (system configuration)

Looking at this order, we can see that if we specify a configuration option in 
our **user** namespace as well as in the **system** namespace, then the key in 
the **user** namespace takes precedence over the one in the **system** namespace. 
If there is no such key in the **user** namespace the key in the **system** 
namespace acts as a fallback.

Lets look at an example:

###### Add a Key to the system Namespace ######

Configuration in the **system** namespace is readable for all users and the same 
for all users. This namespace provides a default or fallback configuration.

In the default Elektra installation only an administrator can update 
configuration here:

```sh
kdb get /sw/tutorial/cascading/#0/current/test
# RET:    1
# STDERR: Did not find key

# Now add the key ...
sudo kdb set system/sw/tutorial/cascading/#0/current/test "hello world"
#> create a new key system/sw/tutorial/cascading/#0/current/test with string hello world

# ... and verify that it exists
kdb get /sw/tutorial/cascading/#0/current/test
#> hello world
```

###### Add a Key to the user Namespace ######

A user may now want to override the configuration in **system**, so he sets a 
key in the **user** namespace:

```sh
kdb set user/sw/tutorial/cascading/#0/current/test "hello galaxy"
#> Create a new key user/sw/tutorial/cascading/#0/current/test with string hello galaxy

# This key masks the key in the system namespace
kdb get /sw/tutorial/cascading/#0/current/test
#> hello galaxy
```

Note that configuration in the **user** namespace only affects _this_ user. 
Other users would still get the key from the **system** namespace.

###### Add a Key to the dir Namespace ######

The **dir** namespace is associated with a directory. The configuration in the 
**dir** namespace applies to the associated directory and all its subdirectories.
This is useful if you have project specific settings (e.g. your git 
configuration or a .htaccess file).

As **dir** precedes the **user** namespace, configuration in **dir** can 
overwrite user configuration:

```sh
# create and change to a new directory ...
mkdir kdbtutorial && cd $_

# ... and create a key in this directories dir-namespace
kdb set dir/sw/tutorial/cascading/#0/current/test "hello universe"
#> Create a new key dir/sw/tutorial/cascading/#0/current/test with string hello universe

# This key masks the key in the system namespace
kdb get /sw/tutorial/cascading/#0/current/test
#> hello universe

# ... and is only present in the associated directory
cd ..
kdb get /sw/tutorial/cascading/#0/current/test
#> hello galaxy
```

###### Add a Key to the proc Namespace ######

The **proc** namespace is only accessible from within applications, not from the 
commandline. Thus we have to omit an example for this namespace at this point.
[Elektrified](/doc/help/elektra-glossary.md) applications can use this namespace 
to override configuration from other namespaces internally.

###### Add a Key to the spec Namespace ######

Because the **spec** namespace does not contain values of keys but their 
metadata, Elektra handles the **spec** namespace differently to other namespaces. 
The following part of the tutorial demonstrates the impact of the **spec** 
namespace on cascading lookups.

## Cascading ##

Cascading triggers actions when, for example, the key isn't found.
Our previous example uses this concept of using `system` configuration
when the `user` configuration is not defined. When a key starts with `/`, a
*cascading lookup* will automatically be performed, e.g. using `/test` instead
of `system/test` will do a cascading lookup.


## Override Links ##

The `spec` namespace is special as it can change how the cascading
lookup works.

For example, in the metadata of the respective `spec`-keys, *override links*
specify to use other keys in favor of the key itself. This way, even
config from current folders (`dir`) can be overwritten.

A cascading lookup considers metadata of `spec`-keys as follows:

 1. `override/#` keys
 2. namespaces specified in the `namespaces/#` keys
 3. Otherwise all namespaces, see [here](/doc/help/elektra-namespaces.md).
 4. `fallback/#` keys
 5. `default` value 

**Note:** `override/#` means an array of `override` keys, fill the array by
          setting `#` followed by the position, e.g. `#0`, `#1`, etc.

As you can see, a lookup considers override links before everything else, which
makes them powerful.

To create an override link, first you need to create a key to link the override
to:

```sh
sudo kdb set system/overrides/test "hello override"
#> Create a new key system/overrides/test with string hello override
```

Define override links by adding them to the `override/#` array:

```sh
sudo kdb setmeta spec/sw/tutorial/cascading/#0/current/test override/#0 /overrides/test
kdb get /sw/tutorial/cascading/#0/current/test
#> hello override
```

Furthermore, you can specify a custom order for the namespaces, set fallback
keys and more. For more information, read the [`elektra-spec` help page](/doc/help/elektra-spec.md).


## User Defaults ##

Override links can also be used to define default values. It is comparable to
defining default values via the `system` namespace, but uses overrides, which
means it is preferred over the configuration in the current folder (`dir`).

This means that user defaults overwrite values specified in the `.configuration`
file we created and mounted earlier in this tutorial.

First you need to create the system default value to link the override to if the
user hasn't defined it:

```sh
$ sudo kdb set system/overrides/test "hello default"
#> Create a new key system/overrides/test with string hello default
```

Then we can create the link:

```sh
sudo kdb setmeta spec/sw/tutorial/cascading/#0/current/test override/#0 /overrides/test
kdb get /sw/tutorial/cascading/#0/current/test
#> hello default
```

Now the user overrides the system default:

```sh
kdb set /overrides/test "hello user"
#> Using name user/overrides/test
#> Create a new key user/overrides/test with string hello user
kdb get /sw/tutorial/cascading/#0/current/test
#> hello user
```
