// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;

/**
 *
 */
class ModuleNameResolver
{
    private final DynamicParameter myCurrentLoadRelativeDirectory;
    private final DynamicParameter myCurrentDirectory;

    ModuleNameResolver(DynamicParameter currentLoadRelativeDirectory,
                       DynamicParameter currentDirectory)
    {
        myCurrentLoadRelativeDirectory = currentLoadRelativeDirectory;
        myCurrentDirectory = currentDirectory;
    }


    File resolve(Evaluator eval, String path)
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

            File baseFile = new File(base);
            assert baseFile.isAbsolute();
            pathFile = new File(base, path);
        }

        return pathFile;
    }
}
