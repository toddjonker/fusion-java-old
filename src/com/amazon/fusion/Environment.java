// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Set;

/**
 * Binds identifiers to {@link FusionValue}s.
 * Environments are generally arranged in a hierarchy of lexical scopes.
 */
interface Environment
{
    interface Binding
    {
        /**
         * Gets the original binding this represents.
         * If this is a free, lexical, or namespace-level binding, return this.
         * If this is an exported module binding, return the internal
         * namespace-level binding.
         *
         * @return not null.
         */
        Binding originalBinding();

        /**
         * @return null if there's no value associated with the binding.
         */
        FusionValue lookup(Environment store);
    }


    Namespace namespace();


    /**
     * NOT RECURSIVE TO ENCLOSING ENVIRONMENTS!
     *
     * @param marks not null.
     *
     * @return given binding if not substituted here; not null.
     */
    Binding substitute(Binding binding, Set<Integer> marks);


    /**
     * Finds the value bound to a given name.
     *
     * @param name must not be null or empty.
     *
     * @return the bound value, or null if there's no value.
     */
    FusionValue lookup(Binding binding);
}
