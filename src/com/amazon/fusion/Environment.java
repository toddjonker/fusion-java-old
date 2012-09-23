// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Set;

/**
 * Maps identifiers to {@link Binding}s.
 * Environments are compile-time entities, arranged in a hierarchy of lexical
 * scopes.
 */
interface Environment
{
    Namespace namespace();


    /** What's the lexical depth?  0 == top-level */
    int getDepth();


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
     * @param binding must not be null.
     *
     * @return the bound value, or null if there's no value.
     */
    Object lookup(Binding binding);
}
