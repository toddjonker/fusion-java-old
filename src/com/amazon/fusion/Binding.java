// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Bindings are used during expansion and compilation to identify a specific
 * binding site.
 * They are compiled away and are not used at eval-time.
 */
interface Binding
{
    String getName();

    /**
     * Determines whether this is a {@link FreeBinding} with the given name.
     */
    boolean isFree(String name);

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
     * Determines whether two bindings refer to the same
     * {@link #originalBinding()}, despite any renames on import or export.
     *
     * @param other must not be null.
     */
    boolean sameTarget(Binding other);

    /**
     * @return null if there's no value associated with the binding.
     */
    Object lookup(Environment store);

    /** Compile a reference to the variable denoted by this binding. */
    CompiledForm compileReference(Evaluator eval, Environment env)
        throws FusionException;

    /** Compile a mutation of the variable denoted by this binding. */
    CompiledForm compileSet(Evaluator eval, Environment env,
                            CompiledForm valueForm)
        throws FusionException;
}
