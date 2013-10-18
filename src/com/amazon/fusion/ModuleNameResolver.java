// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.isValidModulePath;
import static com.amazon.ion.util.IonTextUtils.printString;
import java.util.ArrayList;

/**
 *
 */
final class ModuleNameResolver
{
    /** Private continuation mark to detect module cycles. */
    private static final Object MODULE_LOADING_MARK =
        new DynamicParameter(null);

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
     * @param baseModule the starting point for relative references; not null.
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
     * @param baseModule the starting point for relative references; not null.
     *
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
            // Relative path
            id = ModuleIdentity.locateRelative(baseModule, modulePath);
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
     * @param eval the evaluation context.
     * @param baseModule the starting point for relative references; not null.
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
        buf.append(printString(modulePath));
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


    private void checkForCycles(Evaluator eval, Object moduleId)
        throws FusionException
    {
        ArrayList<Object> marks =
            eval.continuationMarks(MODULE_LOADING_MARK);

        for (int i = 0; i < marks.size(); i++)
        {
            if (marks.get(i).equals(moduleId))
            {
                // Found a cycle!
                StringBuilder message = new StringBuilder();
                message.append("Module dependency cycle detected: ");
                for ( ; i >= 0; i--)
                {
                    message.append(marks.get(i));
                    message.append(" -> ");
                }
                message.append(moduleId);

                throw new FusionException(message.toString());
            }
        }
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
            if (! reg.isLoaded(id))
            {
                Object idString = eval.newString(id.internString());

                checkForCycles(eval, idString);

                Evaluator loadEval =
                    eval.markedContinuation(new Object[]{ myCurrentModuleDeclareName,
                                                          MODULE_LOADING_MARK },
                                            new Object[]{ idString, idString });
                myLoadHandler.loadModule(loadEval, id);
                // Evaluation of 'module' declares it, but doesn't instantiate.
            }
        }

        return id;
    }
}
