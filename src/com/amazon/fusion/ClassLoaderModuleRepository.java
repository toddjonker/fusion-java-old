// Copyright (c) 2012-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import com.amazon.fusion.util.function.Predicate;
import java.net.URL;

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
        final String resourceName =
            "/FUSION-REPO" + path + FUSION_SOURCE_EXTENSION;

        // The protocol could be a jar: or file: (at least!)
        URL url = getClass().getResource(resourceName);
        if (url == null) return null;

        return ModuleLocation.forUrl(id, url);
    }


    @Override
    void collectModules(Predicate<ModuleIdentity> selector,
                        Consumer<ModuleIdentity>  results)
        throws FusionException
    {
        // Nothing to do. We can't introspect the classloader.
        // TODO If the URL points to a Jar, we may be able to read its entries.
        //   Or, perhaps write a manifest/index into the repo root at build time.
    }
}
