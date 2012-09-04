// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.File;

/**
 * Primary entry point for embedding Fusion within a Java program.
 * <p>
 * The runtime maintains a top-level environment within which expressions can
 * be evaluated. The environment is maintained between calls.
 */
public interface FusionRuntime
{

    /**
     * Evaluates top-level forms within this {@link FusionRuntime}'s namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    public FusionValue eval(String source)
        throws ExitException, FusionException;

    /**
     * Evaluates top-level forms within this {@link FusionRuntime}'s namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    public FusionValue eval(IonReader source)
        throws ExitException, FusionException;

    /**
     * Evaluates top-level forms within this {@link FusionRuntime}'s namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    public Object load(File source)
        throws ExitException, FusionException;


    /**
     * Binds an identifier with a value in this {@link FusionRuntime}'s
     * namespace.
     *
     * @param value must not be null.
     */
    public void bind(String name, FusionValue value);


    // TODO require(File)

    // TODO main(String...)
}
