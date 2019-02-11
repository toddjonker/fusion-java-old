// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonCatalog;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * Primary entry point for embedding Fusion within a Java program.
 * The runtime contains resources common to many evaluations, in particular a
 * registry of all loaded modules. Instances are immutable and thread-safe.
 * <p>
 * <b>WARNING:</b> This interface must not be implemented or extended by
 * code outside of this library.
 * <p>
 * The runtime should be considered a very heavyweight component, and it should
 * not be used in a transient or localized fashion. Most applications should
 * construct exactly one runtime.
 * <p>
 * The runtime maintains a {@link TopLevel} namespace within which expressions
 * can be evaluated. The namespace bindings and state are maintained between
 * calls.  Applications that need isolated evaluation should create additional
 * top levels by calling one of the {@link #makeTopLevel} methods.
 * <p>
 * To create a {@link FusionRuntime}, use a {@link FusionRuntimeBuilder}.
 */
public interface FusionRuntime
{
    /**
     * Gets the identifier for this runtime's default language, which supplies
     * the initial bindings for its {@link TopLevel} namespaces.
     * Unless configured otherwise by the {@link FusionRuntimeBuilder}, this
     * value is {@code "/fusion"}.
     *
     * @return an absolute module path.
     *
     * @see FusionRuntimeBuilder#getDefaultLanguage()
     */
    public String getDefaultLanguage();


    /**
     * Gets the Ion symbol table catalog used when reading and writing Ion data.
     *
     * @return not null.
     */
    public IonCatalog getDefaultIonCatalog();


    /**
     * Returns a singular {@link TopLevel} for this runtime; each call returns
     * the same instance.  All uses of the instance will share bindings and
     * state, so applications that require isolated state must create
     * additional {@link TopLevel}s via {@link #makeTopLevel()}.
     * <p>
     * The default {@link TopLevel} is bootstrapped with bindings from this
     * runtime's default language.
     *
     * @return not null.
     *
     * @see #getDefaultLanguage()
     */
    public TopLevel getDefaultTopLevel()
        throws FusionException;


    /**
     * Returns a fresh {@link TopLevel} instance, bootstrapped with this
     * runtime's default language.
     *
     * @return not null.
     *
     * @see #getDefaultLanguage()
     */
    public TopLevel makeTopLevel()
        throws FusionException;


    /**
     * Returns a fresh {@link TopLevel} instance, populated with bindings from
     * a given module.  The default language is <em>not</em> imported.
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
