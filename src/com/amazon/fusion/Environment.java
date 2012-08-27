// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Collection;

/**
 * Binds names to {@link FusionValue}s.
 * Environments are generally arranged in a hierarchy of lexical scopes.
 */
interface Environment
{
    interface Binding {
        FusionValue lookup(Environment store);
    }


    Namespace namespace();


    Binding resolve(String name);


    /**
     * Finds the value bound to a given name.
     *
     * @param name must not be null or empty.
     *
     * @return the bound value, or null if there's no binding.
     */
    FusionValue lookup(String name);

    /**
     * Adds all names visible in this environment to a collection.
     *
     * @param names must not be null.
     */
    void collectNames(Collection<String> names);
}
