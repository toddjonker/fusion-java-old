// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Bindings are used at prepare-time to identify a specific binding site.
 * They are compiled away and are not used at eval-time.
 */
public interface Binding
{
    String getName();

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

    /** Compile a reference to the variable denoted by this binding. */
    CompiledForm compileReference(Evaluator eval, Environment env)
        throws FusionException;
}
