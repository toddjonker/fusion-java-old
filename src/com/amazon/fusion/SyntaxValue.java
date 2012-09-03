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


    private final String[] myAnnotations;
    private final SourceLocation mySrcLoc;

    /**
     * The sequence of wraps around this value.
     * Wraps only affect symbols; a wrapped symbol is an identifier.
     * If this is a container, any wraps are just being held here lazily,
     * waiting to be pushed down to all children (prepended onto their existing
     * wraps) once any are requested.
     */
    SyntaxWraps myWraps;  // FIXME should be immutable

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

    SyntaxValue(String[] annotations, SourceLocation loc, SyntaxWraps wraps)
    {
        assert annotations != null : "annotations must not be null";
        myAnnotations = annotations;
        mySrcLoc = loc;
        myWraps = wraps;
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


    SyntaxValue addWrap(SyntaxWrap wrap)
    {
        if (myWraps == null)
        {
            myWraps = SyntaxWraps.make(wrap);
        }
        else
        {
            myWraps = myWraps.addWrap(wrap);
        }
        return this;  // TODO fixme!
    }

    /**
     * Prepends a sequence of wraps onto our existing wraps.
     * It is assumed that the given list will not be modified later and can
     * therefore be shared.
     */
    SyntaxValue addWraps(SyntaxWraps wraps)
    {
        if (myWraps == null)
        {
            myWraps = wraps;
        }
        else
        {
            myWraps = myWraps.addWraps(wraps);
        }
        return this;  // TODO fixme!
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
