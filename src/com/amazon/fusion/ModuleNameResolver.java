// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
}
