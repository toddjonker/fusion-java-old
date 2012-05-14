// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonValue;
import java.io.File;

/**
 *
 */
class ModuleNameResolver
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


    ModuleIdentity resolve(Evaluator eval, Environment env, IonValue pathStx)
        throws FusionException
    {
        switch (pathStx.getType())
        {
            case STRING:
            {
                String path = ((IonString) pathStx).stringValue();
                return resolve(eval, env, path);
            }
            case SEXP:
            {
                IonSexp pathSexp = (IonSexp) pathStx;
                return resolve(eval, env, pathSexp);
            }
        }

        throw new SyntaxFailure("module path", "unrecognized form", pathStx);
    }


    ModuleIdentity resolve(Evaluator eval, Environment env, IonSexp pathStx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker("module path", pathStx);
        check.arityExact(2);

        String form = check.requiredNonEmptySymbol("symbol", 0);
        if ("lib".equals(form))
        {
            // TODO libName should not be absolute
            String libName = check.requiredNonEmptyString("module name", 1);
            libName += ".ion"; // TODO ugly hard-coding
            File repo = findRepository();
            File pathFile = new File(repo, libName);
            return resolve(eval, env, pathFile);
        }

        throw check.failure("unrecognized form");
    }


    ModuleIdentity resolve(Evaluator eval, Environment env, String path)
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

        return resolve(eval, env, pathFile);
    }


    private ModuleIdentity resolve(Evaluator eval, Environment env,
                                   File pathFile)
        throws FusionException
    {
        ModuleIdentity id = ModuleIdentity.intern(pathFile);

        Namespace ns = env.namespace();
        ModuleRegistry reg = ns.getRegistry();
        if (reg.lookup(id) == null)
        {
            FusionValue idString = eval.newString(id.toString());
            Evaluator loadEval =
                eval.markedContinuation(myCurrentModuleDeclareName, idString);
            ModuleInstance module =
                myLoadHandler.loadModule(loadEval, ns, pathFile);
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
