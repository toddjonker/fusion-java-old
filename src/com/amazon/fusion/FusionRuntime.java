// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * Primary entry point for embedding Fusion within a Java program.
 * The runtime contains resource common to many evaluations, in particular the
 * registry of built-in modules. Most applications are expected to construct
 * exactly one runtime, and probably multiple {@link TopLevel}s to isolate
 * state between different code fragments.
 * <p>
 * The runtime maintains a {@link TopLevel} namespace within which expressions
 * can be evaluated. The namespace bindings and state are maintained between
 * calls.  Applications that need isolated evaluation should create additional
 * top levels by calling one of the {@link #makeTopLevel} methods.
 * <p>
 * To create a {@link FusionRuntime}, use a {@link FusionRuntimeBuilder}.
 * <p>
 * <b>WARNING:</b> This interface should not be implemented or extended by
 * code outside of this library.
 */
public interface FusionRuntime
{
    /**
     * Returns a singular {@link TopLevel} for this runtime; each call returns
     * the same instance.  All uses of the instance will share bindings and
     * state, so applications that require isolated state must create
     * additional {@link TopLevel}s via {@link #makeTopLevel()}.
     *
     * @return not null.
     */
    public TopLevel getDefaultTopLevel()
        throws FusionException;

    /**
     * Returns a fresh {@link TopLevel} instance.
     *
     * @return not null.
     */
    public TopLevel makeTopLevel()
        throws FusionException;


    /**
     * Returns a fresh {@link TopLevel} instance, populated with bindings from
     * a given module.
     *
     * @param initialModulePath must be an absolute module path, starting with
     * {@code '/'}.
     *
     * @return not null.
     */
    public TopLevel makeTopLevel(String initialModulePath)
        throws FusionException;


    /**
     * Returns a builder for constructing modules via Java.
     *
     * @param absoluteModulePath must be an absolute module path, starting
     * with {@code '/'}.
     * @return a new module builder.
     */
    public ModuleBuilder makeModuleBuilder(String absoluteModulePath)
        throws FusionException;


    //========================================================================


    /**
     * Creates a fresh {@code IonValue} DOM from a Fusion value, using the
     * default ionization strategy.
     *
     * @param factory must not be null.
     *
     * @return a fresh instance, without a container.
     *
     * @throws FusionException if the value is not handled by the default
     * ionization strategy, or if something else goes wrong during ionization.
     */
    public IonValue ionize(Object fusionValue, ValueFactory factory)
        throws FusionException;


    /**
     * Creates a fresh {@code IonValue} DOM from a Fusion value, using the
     * default ionization strategy.
     *
     * @param factory must not be null.
     *
     * @return a fresh instance, without a container, or null if the value is
     * not handled by the default ionization strategy.
     *
     * @throws FusionException if something goes wrong during ionization.
     */
    public IonValue ionizeMaybe(Object fusionValue, ValueFactory factory)
        throws FusionException;
}
