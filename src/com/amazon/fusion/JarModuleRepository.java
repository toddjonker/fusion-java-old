// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

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
                // TODO Maybe not the best output this way.
                private final SourceName myName =
                    SourceName.forDisplay(id + " (at classpath:" + fileName
                                             + ")");

                @Override
                SourceName sourceName()
                {
                    return myName;
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


    @Override
    void collectModules(Predicate<ModuleIdentity> selector,
                        Consumer<ModuleIdentity>  results)
        throws FusionException
    {
        // Nothing to do. We can't introspect the classloader.
    }
}
