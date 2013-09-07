// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.isValidModulePath;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import static com.amazon.ion.util.IonTextUtils.printString;
import java.io.File;
import java.io.IOException;

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


    /**
     * Locates and loads a module, dispatching on the concrete syntax of the
     * request.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    @Deprecated
    ModuleIdentity resolve(Evaluator eval, SyntaxValue pathStx)
        throws FusionException, ModuleNotFoundException
    {
        switch (pathStx.getType())
        {
            case SYMBOL:
            {
                String libName = ((SyntaxSymbol) pathStx).stringValue();
                // TODO check null/empty
                return resolveLib(eval, libName, pathStx);
            }
            case STRING:
            {
                String path = ((SyntaxString) pathStx).stringValue();
                return resolve(eval, path, pathStx);
            }
            case SEXP:
            {
                SyntaxSexp pathSexp = (SyntaxSexp) pathStx;
                return resolve(eval, pathSexp);
            }
        }

        throw new SyntaxException("module path", "unrecognized form", pathStx);
    }

    /**
     * Locates and loads a module, dispatching on the concrete syntax of the
     * request.
     *
     * @param baseModule the starting point for relative references.
     * If null, it indicates a reference from top-level.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    ModuleIdentity resolve(Evaluator eval,
                           ModuleIdentity baseModule,
                           SyntaxValue pathStx,
                           boolean load)
        throws FusionException, ModuleNotFoundException
    {
        switch (pathStx.getType())
        {
            case STRING:
            case SYMBOL:
            {
                String path = ((SyntaxText) pathStx).stringValue();
                // TODO check null/empty
                return resolveModulePath(eval, baseModule, path, load, pathStx);
            }
        }

        throw new SyntaxException("module path", "unrecognized form", pathStx);
    }

    /**
     * Locates and loads a module, dispatching on the concrete syntax of the
     * request.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    @Deprecated
    ModuleIdentity resolve(Evaluator eval, SyntaxSexp pathStx)
        throws FusionException, ModuleNotFoundException
    {
        SyntaxChecker check = new SyntaxChecker(eval, "module path", pathStx);
        check.arityExact(2);

        String form = check.requiredIdentifier("symbol", 0).stringValue();
        if ("lib".equals(form))
        {
            String libName = check.requiredNonEmptyString("module name", 1);
            return resolveLib(eval, libName, pathStx);
        }

        if ("quote".equals(form))
        {
            SyntaxSymbol name = check.requiredSymbol("module name", 1);

            ModuleRegistry reg = eval.findCurrentNamespace().getRegistry();

            ModuleIdentity id;
            if (ModuleIdentity.isValidBuiltinName(name.stringValue()))
            {
                id = ModuleIdentity.internBuiltinName(name.stringValue());
            }
            else
            {
                // These names are scoped by registry!
                ModuleIdentity.validateLocalName(name);
                id = ModuleIdentity.locateLocal(reg, name.stringValue());
            }

            if (id == null || reg.lookup(id) == null)
            {
                throw new ModuleNotFoundException("Module not found", pathStx);
            }
            return id;
        }

        throw check.failure("unrecognized form");
    }


    /**
     * @return null if the referenced module couldn't be located in the current
     * registry or any repository.
     */
    ModuleIdentity locate(Evaluator eval, ModuleIdentity baseModule,
                          String modulePath, SyntaxValue stxForErrors)
        throws FusionException
    {
        ModuleIdentity id;
        if (modulePath.startsWith("/"))
        {
            id = ModuleIdentity.locate(modulePath);
        }
        else
        {
            // TODO FUSION-152 Support relative module paths other than locals
            ModuleRegistry reg = eval.findCurrentNamespace().getRegistry();
            id = ModuleIdentity.locateLocal(reg, modulePath);

            // We can't fall through: repositories wants an absolute path.
            return id;
        }

        if (id == null)
        {
            for (ModuleRepository repo : myRepositories)
            {
                id = repo.resolveLib(eval, modulePath);
                if (id != null) break;
            }
        }
        return id;
    }


    /**
     * Locates and loads a module from the registered repositories.
     *
     * @param libName is always treated as an absolute module path.
     * @param stx is used for error messaging; may be null.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    @Deprecated
    ModuleIdentity resolveLib(Evaluator eval, String libName,
                              SyntaxValue stx)
        throws FusionException, ModuleNotFoundException
    {
        // TODO FUSION-74 Support relative module paths
        if (! libName.startsWith("/")) libName = "/" + libName;

        ModuleIdentity id = locate(eval, null, libName, stx);
        if (id != null)
        {
            return loadModule(eval, id);
        }

        StringBuilder buf = new StringBuilder();
        buf.append("A module named ");
        buf.append(printQuotedSymbol(libName));
        buf.append(" could not be found in the registered repositories.");
        buf.append(" The repositories are:\n");
        for (ModuleRepository repo : myRepositories)
        {
            buf.append("  * ");
            buf.append(repo.identify());
            buf.append('\n');
        }
        String message = buf.toString();

        if (stx == null)
        {
            throw new ModuleNotFoundException(message);
        }
        else
        {
            throw new ModuleNotFoundException(message, stx);
        }
    }

    /**
     * Locates and loads a module from the registered repositories.
     *
     * @param eval the evaluation context.
     * @param baseModule the starting point for relative references.
     * If null, it indicates a reference from top-level.
     * @param modulePath must be a module path.
     * @param load should we load the module, or just determine its identity?
     * @param stxForErrors is used for error messaging; may be null.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    ModuleIdentity resolveModulePath(Evaluator eval,
                                     ModuleIdentity baseModule,
                                     String modulePath,
                                     boolean load,
                                     SyntaxValue stxForErrors)
        throws FusionException, ModuleNotFoundException
    {
        if (! isValidModulePath(modulePath))
        {
            String message = "Invalid module path: " + printString(modulePath);
            throw new SyntaxException(null, message, stxForErrors);
        }

        ModuleIdentity id = locate(eval, baseModule, modulePath, stxForErrors);
        if (id != null)
        {
            if (load) loadModule(eval, id);
            return id;
        }

        StringBuilder buf = new StringBuilder();
        buf.append("A module named ");
        buf.append(printQuotedSymbol(modulePath));
        buf.append(" could not be found in the registered repositories.");
        buf.append(" The repositories are:\n");
        for (ModuleRepository repo : myRepositories)
        {
            buf.append("  * ");
            buf.append(repo.identify());
            buf.append('\n');
        }
        String message = buf.toString();

        if (stxForErrors == null)
        {
            throw new ModuleNotFoundException(message);
        }
        else
        {
            throw new ModuleNotFoundException(message, stxForErrors);
        }
    }


    /**
     * Resolve a file path to a module identity and load the module into the
     * current {@link ModuleRegistry}.
     *
     * @param eval the current evaluation context; not null.
     * @param path the file to resolve and load. If relative, its resolved
     * relative to the {@code current_load_relative_directory} parameter if
     * its set, or else the {@code current_directory} parameter.
     * @param stxForErrors is used for error messaging
     *
     * @return the identity of the loaded module.
     *
     * @throws ModuleNotFoundException if the module could not be found.
     */
    @Deprecated
    ModuleIdentity resolve(Evaluator eval, String path,
                           SyntaxValue stxForErrors)
        throws FusionException, ModuleNotFoundException
    {
        // TODO FUSION-159 Remove support for .ion extension
        String pathFileName = path.endsWith(".ion") ? path : path + ".ion";

        File pathFile = new File(pathFileName);
        if (! pathFile.isAbsolute())
        {
            // TODO FUSION-74 if we're loading from within a module, the
            //  requested path should be resolve relative to the requiring
            //  module, not to these directories.

            String base = myCurrentLoadRelativeDirectory.asString(eval);
            if (base == null)
            {
                base = myCurrentDirectory.asString(eval);
            }

            // TODO FUSION-50 parameter guard should ensure this
            File baseFile = new File(base);
            assert baseFile.isAbsolute() : "Base is not absolute: " + baseFile;
            pathFile = new File(base, pathFileName);
        }

        if (pathFile.exists())
        {
            // TODO FUSION-74 Is this correct in all cases?
            String modulePath;
            try
            {
                modulePath = pathFile.getCanonicalPath();
            }
            catch (IOException e)
            {
                throw new FusionException("Unable to resolve file system path "
                                          + pathFile);
            }
            ModuleIdentity id = ModuleIdentity.internFromFile(modulePath,
                                                              pathFile);
            return loadModule(eval, id);
        }

        String message =
            "A module file could not be found at the requested path " +
            printString(path)+ "\n" +
            "The syntax in use looks for a relative file, and does not " +
            "search any registered repositories.";
        // TODO explain where we looked
        throw new ModuleNotFoundException(message, stxForErrors);
    }


    private ModuleIdentity loadModule(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        // TODO Need a way to resolve only, avoid loading, as per Racket.

        ModuleRegistry reg = eval.findCurrentNamespace().getRegistry();

        // Ensure that we don't try to load the module twice simultaneously.
        // TODO FUSION-73 This is probably far too coarse-grained in general.
        synchronized (reg)
        {
            if (reg.lookup(id) == null)
            {
                Object idString = eval.newString(id.internString());
                Evaluator loadEval =
                    eval.markedContinuation(myCurrentModuleDeclareName, idString);
                myLoadHandler.loadModule(loadEval, id);
                // Evaluation of 'module' registers the ModuleInstance
            }
        }

        return id;
    }
}
