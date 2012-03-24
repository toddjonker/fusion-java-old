// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 * The core features of a Fusion run-time value.  Note that the set of Fusion
 * values is a superset of the Ion values, so not all {@link FusionValue}s are
 * Ion data values.
 */
abstract class FusionValue
{
    /** A zero-length array. */
    public static final FusionValue[] EMPTY_ARRAY = new FusionValue[0];


    /**
     * Gets an Ion representation of this value, if available.
     * <p>
     * TODO This should be refactored.
     *
     * @return may be null.
     */
    IonValue getDom()
    {
        return null;
    }


    /**
     * Prints a representation of this value for human consumption, generally
     * for use by "print" phase of the {@linkplain Repl Read-Eval-Print Loop}.
     * <p>
     * TODO This should be refactored.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    abstract void print(Writer out)
        throws IOException;


    /**
     * Prints the documentation of this value.
     * Implementations should try to ensure that a final newline is printed.
     * <p>
     * TODO This API should be refactored to use a Documentation abstraction
     * that can be output in various ways.
     *
     * @param out the output stream; not null.
     *
     * @throws IOException Propagated from the output stream.
     */
    void printDoc(Writer out)
        throws IOException
    {
        out.write("// No documentation.\n");
    }


    /**
     * Invokes the value as the first position in an S-expression.
     *
     * @param eval the evaluation engine to use for sub-expressions; not null.
     * @param env the lexical environment, containing visible bindings;
     *  not null.
     * @param expr the source expression being invoked; not null.
     *  The first child element is the source for this value.
     *
     * @return the invocation result.
     */
    abstract FusionValue invoke(Evaluator eval,
                                Environment env,
                                IonSexp expr);
}
