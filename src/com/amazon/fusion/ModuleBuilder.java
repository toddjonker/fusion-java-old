// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Constructs "built-in" modules via Java code.
 * <p>
 * Applications should use this with great discretion and hesitation, since
 * it leads to non-portable Fusion code.
 * <p>
 * To create a {@link ModuleBuilder}, use
 * {@link FusionRuntime#makeModuleBuilder(String)}.
 */
public interface ModuleBuilder
{
    /**
     * Creates or alters a top-level definition in the module to be built.
     *
     * @param name must be non-empty.
     * @param value must be acceptable to the Fusion runtime.
     */
    void define(String name, Object value)
        throws FusionException;


    /**
     * Instatiates the module for use from Fusion code.
     * <p>
     * This builder must be discarded after this method is called.
     *
     * @throws FusionException if anything goes wrong.
     */
    void instantiate()
        throws FusionException;
}
