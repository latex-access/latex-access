"""Various utilities used for importing modules programmatically."""


def module_exists(mod_name, package):
    """Check whether module with a given name exists in a specified package.

    We cannot just try to import a given module,
    and assume that if `ImportError was raised it does not exist,
    since it may also be raised from the module we are trying to import
    (for  example due to another module being missing,
    or name in an import statement being misspelled).
    To use pass the name of the module, and the package
    (an actual module, not its name!) in which the module should be located.
    Free standing modules i.e. those not inside a package are not supported.
    """
    pkg_name = package.__name__
    qualified_name = "{0}.{1}".format(pkg_name, mod_name)
    try:
        # Import `find_spec`` by name,
        # since `importli.util`` was introduced before this function.
        from importlib.util import find_spec
    except ImportError:
        # We are on Python 2,
        # where importlib's functionality was extremely limited.
        # Just use pkgutil.
        import pkgutil
        importers_list = list(
            pkgutil.iter_importers("{}.__init__".format(pkg_name))
        )
        return any(
            i.find_module(qualified_name) for i in importers_list
        )
    else:
        try:
            return bool(find_spec(
                qualified_name,
                package=package
            ))
        except ImportError:
            # Handle a case where provided name of the module contains a dot.
            # In that case `importlib`` believes that
            # name up to the first dot is a sub-package,
            # and attempts to import it.
            # Since an `ImportError`` was raised,
            # meaning such sub-package does not exist,
            # the requested module does not exist either.
            return False


def import_module(mod_name, package):
    """Import module with a given name from a specified package.

    Note that this function intentionally does not catch any exceptions;
    It is assumed that either existence of the module was verified
    by calling `module_exists`First, or that exceptions raised during import
    should be propagated to, and handled by the caller.
    """
    pkg_name = package.__name__
    import importlib
    return importlib.import_module(
        "{0}.{1}".format(pkg_name, mod_name),
        package=pkg_name
    )
