// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;

/**
 * Models Fusion source code, using a custom DOM implementation of Ion.
 * Unlike the {@link IonValue} model, this one allows sharing of nodes in a
 * DAG structure.
 */
abstract class SyntaxValue
    extends FusionValue
{
    enum Type {
        NULL, BOOL, INT, DECIMAL, FLOAT, TIMESTAMP, BLOB, CLOB,
        STRING, SYMBOL, LIST, SEXP, STRUCT
    }

    /** A zero-lengeth array. */
    final static SyntaxValue[] EMPTY_ARRAY = new SyntaxValue[0];

    private final String[] myAnnotations;
    private final SourceLocation mySrcLoc;

    /**
     *
     * @param annotations the new instance assumes ownership of the array and
     * it must not be modified later. Must not be null.
     *
     * @param loc may be null;
     */
    SyntaxValue(String[] annotations, SourceLocation loc)
    {
        assert annotations != null : "annotations must not be null";
        myAnnotations = annotations;
        mySrcLoc = loc;
    }


    final String[] getAnnotations()
    {
        return myAnnotations;
    }

    /**
     * Gets the location associated with this syntax node, if it exists.
     * @return may be null.
     */
    SourceLocation getLocation()
    {
        return mySrcLoc;
    }

    abstract Type getType();


    /**
     * Prepends a wrap onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWrap(SyntaxWrap wrap)
    {
        return this;
    }

    /**
     * Prepends a sequence of wraps onto our existing wraps.
     * This will return a new instance as necessary to preserve immutability.
     */
    SyntaxValue addWraps(SyntaxWraps wraps)
    {
        return this;
    }


    SyntaxValue addOrRemoveMark(int mark)
    {
        SyntaxWrap wrap = new MarkWrap(mark);
        return addWrap(wrap);
    }

    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        return this;
    }

    abstract FusionValue eval(Evaluator eval, Environment env)
        throws FusionException;

    /**
     * Transform this syntax into plain values.
     */
    abstract FusionValue quote(Evaluator eval);

    @Override
    void write(Appendable out)
        throws IOException
    {
        IonWriter writer = IonTextWriterBuilder.standard().build(out);
        writeTo(writer);
        writer.flush();
    }


    /**
     * Writes this syntax in Ion form.
     * @param writer must not be null.
     */
    final void writeTo(IonWriter writer)
        throws IOException
    {
        writer.setTypeAnnotations(myAnnotations);
        writeContentTo(writer);
    }

    /** Write the content (not including annotations) to a writer. */
    abstract void writeContentTo(IonWriter writer)
        throws IOException;

    abstract boolean isNullValue();
}
