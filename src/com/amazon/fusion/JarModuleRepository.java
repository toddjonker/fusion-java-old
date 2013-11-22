// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import java.io.IOException;
import java.io.InputStream;

final class JarModuleRepository
    extends ModuleRepository
{
    @Override
    String identify()
    {
        return "ClassLoader repository";
    }


    @Override
    ModuleLocation locateModule(Evaluator eval, final ModuleIdentity id)
        throws FusionException
    {
        String path = id.absolutePath();
        final String fileName =
            "/FUSION-REPO" + path + FUSION_SOURCE_EXTENSION;

        if (getClass().getResource(fileName) != null)
        {
            ModuleLocation loc = new InputStreamModuleLocation()
            {
                @Override
                SourceName sourceName()
                {
                    // TODO Maybe not the best output this way.
                    String name = id + " (at classpath:" + fileName + ")";
                    return SourceName.forDisplay(name);
                }

                @Override
                InputStream open()
                    throws IOException
                {
                    return getClass().getResourceAsStream(fileName);
                }
            };

            return loc;
        }

        return null;
    }
}
