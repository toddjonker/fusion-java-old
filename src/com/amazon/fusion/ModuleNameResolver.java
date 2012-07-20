// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
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

    ModuleNameResolver(LoadHandler loadHandler,
                       DynamicParameter currentLoadRelativeDirectory,
                       DynamicParameter currentDirectory,
                       DynamicParameter currentModuleDeclareName)
    {
        myLoadHandler = loadHandler;
        myCurrentLoadRelativeDirectory = currentLoadRelativeDirectory;
        myCurrentDirectory = currentDirectory;
        myCurrentModuleDeclareName = currentModuleDeclareName;
    }


    ModuleIdentity resolve(Evaluator eval, IonValue pathStx)
        throws FusionException
    {
        switch (pathStx.getType())
        {
            case SYMBOL:
            {
                String libName = ((IonSymbol) pathStx).stringValue();
                // TODO check null/empty
                return resolveLib(eval, libName);
            }
            case STRING:
            {
                String path = ((IonString) pathStx).stringValue();
                return resolve(eval, path);
            }
            case SEXP:
            {
                IonSexp pathSexp = (IonSexp) pathStx;
                return resolve(eval, pathSexp);
            }
        }

        throw new SyntaxFailure("module path", "unrecognized form", pathStx);
    }


    ModuleIdentity resolve(Evaluator eval, IonSexp pathStx)
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
        // TODO libName should not be absolute
        String fileName = libName + ".ion"; // TODO ugly hard-coding

        if (getClass().getResource("/FUSION-REPO/" + fileName) != null)
        {
            ModuleIdentity id = ModuleIdentity.internFromJar(fileName);
            return resolve(eval, id);
        }

        File repo = findRepository();
        File pathFile = new File(repo, fileName);
        if (! pathFile.exists())
        {
            String message =
                "Library not found: " + printString(libName);
            throw new FusionException(message);
        }

        return resolve(eval, pathFile);
    }


    ModuleIdentity resolve(Evaluator eval, String path)
        throws FusionException
    {
        if (! path.endsWith(".ion")) path += ".ion";

        File pathFile = new File(path);
        if (! pathFile.isAbsolute())
        {
            String base = myCurrentLoadRelativeDirectory.asString(eval);
            if (base == null)
            {
                base = myCurrentDirectory.asString(eval);
            }

            // TODO The parameters should guard their values to ensure this
            File baseFile = new File(base);
            assert baseFile.isAbsolute() : "Base is not absolute: " + baseFile;
            pathFile = new File(base, path);
        }

        return resolve(eval, pathFile);
    }


    private ModuleIdentity resolve(Evaluator eval, File pathFile) // inline
        throws FusionException
    {
        ModuleIdentity id = ModuleIdentity.intern(pathFile);
        return resolve(eval, id);
    }

    private ModuleIdentity resolve(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        ModuleRegistry reg = eval.getModuleRegistry();
        if (reg.lookup(id) == null)
        {
            FusionValue idString = eval.newString(id.internString());
            Evaluator loadEval =
                eval.markedContinuation(myCurrentModuleDeclareName, idString);
            ModuleInstance module =
                myLoadHandler.loadModule(loadEval, id);
            reg.register(module);
        }

        return id;
    }


    File findRepository()
    {
        // TODO this is really the wrong place to have this logic
        String fusionRepoDir = System.getProperty("com.amazon.fusion.repoDir");
        if (fusionRepoDir != null)
        {
            File repo = new File(fusionRepoDir);
            assert repo.isAbsolute()
                : "com.amazon.fusion.repoDir is not absolute: " + repo;

            return repo;
        }

        File home = findFusionHomeDir();
        File repo = new File(home, "repo");
        return repo;
    }

    File findFusionHomeDir()
    {
        // TODO this is really the wrong place to have this logic
        // TODO error handling
        String fusionHomeDir = System.getProperty("com.amazon.fusion.home");
        if (fusionHomeDir == null)
        {
            fusionHomeDir = System.getProperty("user.dir");
        }

        return new File(fusionHomeDir);
    }
}
