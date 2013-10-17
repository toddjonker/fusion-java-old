// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.File;

/**
 * A top-level namespace for evaluating Fusion scripts or expressions.
 * Each instance has a separate namespace, bootstrapped with bindings from a
 * specific Fusion module or language.
 * <p>
 * <b>WARNING:</b> This interface must not be implemented or extended by
 * code outside of this library.
 * <p>
 * {@link TopLevel}s are <em>not</em> inherently thread-safe! It is not safe
 * to use the same instance from multiple threads, unless all such use is
 * functional (that is, no mutations are made) with respect to any common data.
 * Put another way, this library does not perform any synchronization on
 * namespaces or user-visibile data structures, so your application must take
 * steps to handle such synchronization when and if it is needed.
 * <p>
 * To create a {@link TopLevel}, use a {@link FusionRuntime}.
 */
public interface TopLevel
{
    /**
     * Evaluates top-level forms within this namespace.
     * Top-level {@code define} forms will alter the environment and will be
     * visible to later calls.
     * <p>
     * If the reader is already positioned on a value (that is, if
     * {@code source.getType() != null}), then that value is the first form
     * evaluated. Otherwise, the reader is advanced to the next value
     * automatically.
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
     * If the reader is already positioned on a value (that is, if
     * {@code source.getType() != null}), then that value is the first form
     * evaluated. Otherwise, the reader is advanced to the next value
     * automatically.
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     * @param name identifies the source for error reporting. May be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     *
     * @deprecated Use {@link #eval(IonReader,SourceName)} instead.
     */
    @Deprecated
    Object load(IonReader source, SourceName name)
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
     *
     * @deprecated Use {@link #eval(IonReader)} instead.
     */
    @Deprecated
    public Object load(IonReader source)
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
     *
     * @param source Fusion source code, in Ion data format. Must not be null.
     * @param name identifies the source for error reporting. May be null.
     *
     * @return the resulting Fusion value; typically the value of the last
     * expression in the source. May be null (if no value results) or an
     * {@code Object[]} (if there are multiple values).
     *
     * @throws ExitException if the Fusion {@code exit} procedure is invoked.
     *
     * @deprecated Use {@link #eval(String,SourceName)} instead
     */
    @Deprecated
    public Object load(String source, SourceName name)
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
     *
     * @deprecated Use {@link #eval(String)} instead.
     */
    @Deprecated
    public Object load(String source)
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
     * Imports all exported bindings from a module into this namespace.
     * <p>
     * If any imported names have top-level definitions, those definitions will
     * be shadowed by the imported binding.
     *
     * @param modulePath locates the required module. It may be either an
     * absolute or relative path.
     */
    public void requireModule(String modulePath)
        throws FusionException;


    /**
     * Binds an identifier with a value in this namespace,
     * <a href="{@docRoot}/overview-summary.html#inject">injecting</a> Java
     * values.
     * <p>
     * If the name has already been defined in this namespace then the existing
     * variable is mutated to have the given value.
     * If the name has been previously imported into this namespace, then a
     * top-level definition is created or modified and will shadow the imported
     * binding in future expressions.
     *
     * @param value the value to bind, must be
     * <a href="{@docRoot}/overview-summary.html#inject">injectable</a>.
     * It is unspecified whether the value will be copied in whole or in part.
     */
    public void define(String name, Object value)
        throws FusionException;


    /**
     * Calls a Fusion procedure by name,
     * <a href="{@docRoot}/overview-summary.html#inject">injecting</a> Java
     * values.
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
