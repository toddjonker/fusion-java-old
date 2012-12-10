// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorIterate;
import static com.amazon.fusion.Iterators.iterateIonSequence;
import com.amazon.ion.IonSequence;
import java.io.IOException;

// TODO Add abstract class so subclasses don't have blank procs?

class FusionIterator
    extends FusionValue
{
    static FusionIterator checkArg(Procedure who, int argNum, Object... args)
        throws ArgTypeFailure
    {
        return who.checkArg(FusionIterator.class, "iterator", argNum, args);
    }


    static FusionIterator iterate(Evaluator eval, Object value)
        throws FusionException
    {
        if (value instanceof FusionIterator)
        {
            return (FusionIterator) value;
        }

        if (isVector(eval, value))
        {
            return (FusionIterator) unsafeVectorIterate(eval, value);
        }

        if (isSexp(eval, value))
        {
            return FusionSexp.unsafeSexpIterate(eval, value);
        }

        if (value instanceof IonSequence)
        {
            return (FusionIterator) iterateIonSequence((IonSequence) value);
        }

        throw new ArgTypeFailure("iterate", "iterable", 0, value);
    }

    static boolean allHaveNext(Evaluator eval, FusionIterator... streams)
        throws FusionException
    {
        for (FusionIterator s : streams)
        {
            if (! s.hasNext(eval)) return false;
        }
        return true;
    }


    private final Procedure myHasNextProc;
    private final Procedure myNextProc;

    FusionIterator(Procedure hasNextProc, Procedure nextProc)
    {
        myHasNextProc = hasNextProc;
        myNextProc    = nextProc;
    }

    /** Public so it can be called from java_new */
    public FusionIterator(Object hasNextProc, Object nextProc)
    {
        myHasNextProc = (Procedure) hasNextProc;
        myNextProc    = (Procedure) nextProc;
    }


    /**
     * For calling from Procedure implementations
     * @param eval
     * @return Fusion true or false (not null.boolean)
     */
    Object doHasNextTail(Evaluator eval)
        throws FusionException
    {
        Object o = eval.callNonTail(myHasNextProc);
        if (FusionValue.asBoolean(o) == null)
        {
            throw new ResultFailure("iterator has_next_proc",
                                    "true or false", o);
        }
        return o;
    }

    boolean hasNext(Evaluator eval) throws FusionException
    {
        Object  o = eval.callNonTail(myHasNextProc, EMPTY_OBJECT_ARRAY);
        Boolean b = FusionValue.asBoolean(o);
        if (b == null)
        {
            throw new ResultFailure("iterator has_next_proc", "true or false",
                                    o);
        }
        return b;
    }

    Object doNextTail(Evaluator eval)
        throws FusionException
    {
        return eval.bounceTailCall(myNextProc);
    }

    Object next(Evaluator eval)
        throws FusionException
    {
        return eval.callNonTail(myNextProc);
    }


    @Override
    void write(Evaluator eval, Appendable out) throws IOException
    {
        out.append("{{{ iterator }}}");
    }


    //========================================================================


    static final class IsIteratorProc
        extends Procedure1
    {
        IsIteratorProc()
        {
            //    "                                                                               |
            super("Checks if the input argument is an iterator. Returns true if it is, and false\n" +
                  "otherwise.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean b = (arg instanceof FusionIterator);
            return eval.newBool(b);
        }
    }


    static final class HasNextProc
        extends Procedure1
    {
        HasNextProc()
        {
            super("Returns whether there are values left to fetch in the ITERATOR.",
                  "iterator");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            FusionIterator iter = FusionIterator.checkArg(this, 0, arg);
            return iter.doHasNextTail(eval);
        }
    }


    static final class NextProc
        extends Procedure1
    {
        NextProc()
        {
            super("Returns the next element of the ITERATOR.",
                  "iterator");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            FusionIterator iter = FusionIterator.checkArg(this, 0, arg);
            return iter.doNextTail(eval);
        }
    }
}
