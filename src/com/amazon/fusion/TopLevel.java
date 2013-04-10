// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.File;

/**
 * A top-level namespace for evaluating Fusion scripts or expressions.
 * Each instance has a separate namespace, bootstrapped with bindings from a
 * specific Fusion module or language.
 * <p>
 * To create a {@link TopLevel}, use a {@link FusionRuntime}.
 */
public interface TopLevel
{
    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     * @param name identifies the source for error reporting. May be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    Object eval(IonReader source, SourceName name)
        throws ExitException, FusionException;


    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     * <p>
     * {@link #eval(IonReader,SourceName)} should be preferred to this method,
     * since it can provide better error reporting.
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     *
     * @see #eval(IonReader,SourceName)
     */
    public Object eval(IonReader source)
        throws ExitException, FusionException;


    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     * @param name identifies the source for error reporting. May be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    public Object eval(String source, SourceName name)
        throws ExitException, FusionException;


    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     * <p>
     * {@link #eval(String,SourceName)} should be preferred to this method,
     * since it can provide better error reporting.
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     *
     * @see #eval(String,SourceName)
     */
    public Object eval(String source)
        throws ExitException, FusionException;



    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     *
     * @param source a file containing Fusion source code, in Ion data format.
     * Must not be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     */
    public Object load(File source)
        throws ExitException, FusionException;


    /**
     * Imports the bindings from a named module into this namespace.
     *
     * @param moduleIdentifier names the required module. It may be either an
     * absolute or relative path.
     */
    public void requireModule(String moduleIdentifier)
        throws FusionException;


    /**
     * Binds an identifier with a value in this namespace.
     *
     * @param value must be of a type supported by the Fusion runtime.
     * Must not be null.
     */
    public void define(String name, Object value)
        throws FusionException;


    /**
     * Calls a Fusion procedure by name, converting Java types to Fusion types
     * where possible.
     *
     * @param procedureName must name a visible procedure, either defined in
     * this namespace or imported from a module.
     *
     * @param arguments the procedure arguments. It is unspecified whether the
     * arguments will be copied in whole or in part. Values may be mutated by
     * the procedure.
     *
     * @return the resulting Fusion value.
     * May be null (if no value results) or an {@code Object[]} (if there are
     * multiple values).
     * Note that "no value" is not the same as "returns void".
     */
    public Object call(String procedureName, Object... arguments)
        throws FusionException;
}
