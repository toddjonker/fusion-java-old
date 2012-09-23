// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
import java.io.File;

/**
 *
 */
final class ModuleNameResolver
{
    private final LoadHandler myLoadHandler;
    private final DynamicParameter myCurrentLoadRelativeDirectory;
    private final DynamicParameter myCurrentDirectory;
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleRepository[] myRepositories;


    ModuleNameResolver(LoadHandler loadHandler,
                       DynamicParameter currentLoadRelativeDirectory,
                       DynamicParameter currentDirectory,
                       DynamicParameter currentModuleDeclareName,
                       ModuleRepository[] repositories)
    {
        myLoadHandler = loadHandler;
        myCurrentLoadRelativeDirectory = currentLoadRelativeDirectory;
        myCurrentDirectory = currentDirectory;
        myCurrentModuleDeclareName = currentModuleDeclareName;
        myRepositories = repositories;
    }


    ModuleIdentity resolve(Evaluator eval, SyntaxValue pathStx)
        throws FusionException
    {
        switch (pathStx.getType())
        {
            case SYMBOL:
            {
                String libName = ((SyntaxSymbol) pathStx).stringValue();
                // TODO check null/empty
                return resolveLib(eval, libName);
            }
            case STRING:
            {
                String path = ((SyntaxString) pathStx).stringValue();
                return resolve(eval, path);
            }
            case SEXP:
            {
                SyntaxSexp pathSexp = (SyntaxSexp) pathStx;
                return resolve(eval, pathSexp);
            }
        }

        throw new SyntaxFailure("module path", "unrecognized form", pathStx);
    }


    ModuleIdentity resolve(Evaluator eval, SyntaxSexp pathStx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker("module path", pathStx);
        check.arityExact(2);

        String form = check.requiredNonEmptySymbol("symbol", 0);
        if ("lib".equals(form))
        {
            String libName = check.requiredNonEmptyString("module name", 1);
            return resolveLib(eval, libName);
        }

        if ("quote".equals(form))
        {
            String libName = check.requiredNonEmptySymbol("module name", 1);
            ModuleIdentity id = ModuleIdentity.intern(libName);
            return id;
        }

        throw check.failure("unrecognized form");
    }


    private ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException
    {
        for (ModuleRepository repo : myRepositories)
        {
            ModuleIdentity id = repo.resolveLib(eval, libName);
            if (id != null)
            {
                return loadModule(eval, id);
            }
        }

        String message = "Library not found: " + printString(libName);
        throw new FusionException(message);
    }


    /**
     * Resolve a file path to a module identity and load the module into the
     * current {@link ModuleRegistry}.
     *
     * @param eval the current evaluation context; not null.
     * @param path the file to resolve and load. If relative, its resolved
     * relative to the {@code current_load_relative_directory} parameter if
     * its set, or else the {@code current_directory} parameter.
     *
     * @return the identity of the loaded module.
     */
    ModuleIdentity resolve(Evaluator eval, String path)
        throws FusionException
    {
        if (! path.endsWith(".ion")) path += ".ion";

        File pathFile = new File(path);
        if (! pathFile.isAbsolute())
        {
            // TODO if we're loading a module, this should be relative to it
            //      and not to these directories.

            String base = myCurrentLoadRelativeDirectory.asString(eval);
            if (base == null)
            {
                base = myCurrentDirectory.asString(eval);
            }

            // TODO FUSION-50 parameter guard should ensure this
            File baseFile = new File(base);
            assert baseFile.isAbsolute() : "Base is not absolute: " + baseFile;
            pathFile = new File(base, path);
        }

        ModuleIdentity id = ModuleIdentity.intern(pathFile);
        return loadModule(eval, id);
    }


    private ModuleIdentity loadModule(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        // TODO Need a way to resolve only, avoid loading, as per Racket.

        ModuleRegistry reg = eval.getModuleRegistry();
        if (reg.lookup(id) == null)
        {
            Object idString = eval.newString(id.internString());
            Evaluator loadEval =
                eval.markedContinuation(myCurrentModuleDeclareName, idString);
            ModuleInstance module =
                myLoadHandler.loadModule(loadEval, id);
            reg.register(module);
        }

        return id;
    }
}
