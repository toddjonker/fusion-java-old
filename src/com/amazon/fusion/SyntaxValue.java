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


    private final SourceLocation mySrcLoc;


    SyntaxValue(SourceLocation loc)
    {
        mySrcLoc = loc;
    }


    SourceLocation getLocation()
    {
        return mySrcLoc;
    }

    abstract Type getType();

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


    abstract void writeTo(IonWriter writer)
        throws IOException;

    abstract boolean isNullValue();
}
