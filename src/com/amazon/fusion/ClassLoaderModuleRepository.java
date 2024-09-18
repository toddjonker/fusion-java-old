// Copyright (c) 2012-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import com.amazon.fusion.util.function.Predicate;
import java.io.IOException;
import java.io.InputStream;

final class ClassLoaderModuleRepository
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
            // TODO Maybe not the best output this way.
            SourceName myName =
                SourceName.forDisplay(id + " (at classpath:" + fileName + ")");

            ModuleLocation loc = new InputStreamModuleLocation(myName)
            {
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
