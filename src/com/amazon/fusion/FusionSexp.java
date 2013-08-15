// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.FusionWrite.dispatchIonize;
import static com.amazon.fusion.FusionWrite.dispatchWrite;
import static com.amazon.fusion.FusionWrite.safeWriteToString;
import com.amazon.fusion.FusionIterator.AbstractIterator;
import com.amazon.fusion.FusionSequence.BaseSequence;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;


final class FusionSexp
{
    private FusionSexp() {}


    static final EmptySexp EMPTY_SEXP = new EmptySexp();
    static final NullSexp  NULL_SEXP  = new NullSexp();


    static NullSexp nullSexp(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return NULL_SEXP;
        }

        return new NullSexp(annotations);
    }

    static EmptySexp emptySexp(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return EMPTY_SEXP;
        }

        return new EmptySexp(annotations);
    }


    /**
     * Caller must have injected children.
     * @param values must not be null. This method assumes ownership!
     */
    static Object immutableSexp(Evaluator eval, Object[] values)
    {
        BaseSexp c = EMPTY_SEXP;

        int i = values.length;
        while (i-- != 0)
        {
            c = new ImmutablePair(values[i], c);
        }

        return c;
    }


    static Object pair(Evaluator eval, Object head, Object tail)
    {
        return new ImmutablePair(head, tail);
    }


    static Object pair(Evaluator eval, String[] annotations,
                       Object head, Object tail)
    {
        return new ImmutablePair(annotations, head, tail);
    }


    static Object sexpFromIonSequence(Evaluator eval, IonSequence seq)
    {
        String[] annotations = seq.getTypeAnnotations();
        // TODO FUSION-47 intern annotation text

        if (seq.isNullValue())
        {
            return nullSexp(eval, annotations);
        }

        int i = seq.size();
        if (i == 0)
        {
            return emptySexp(eval, annotations);
        }

        Object sexp = EMPTY_SEXP;
        while (i-- != 0)
        {
            IonValue iv = seq.get(i);
            Object head = eval.inject(iv);
            if (i == 0)
            {
                sexp = pair(eval, annotations, head, sexp);
            }
            else
            {
                sexp = pair(eval, head, sexp);
            }
        }

        return sexp;
    }


    //========================================================================
    // Predicates

    @Deprecated
    static boolean isSexp(Object v)
    {
        return (v instanceof BaseSexp);
    }

    static boolean isSexp(Evaluator eval, Object v)
    {
        return (v instanceof BaseSexp);
    }


    static boolean isNullSexp(Evaluator eval, Object v)
    {
        return (v instanceof NullSexp);
    }

    static boolean isPair(Evaluator eval, Object v)
    {
        return (v instanceof ImmutablePair);
    }


    //========================================================================
    // Accessors


    static int unsafeSexpSize(Evaluator eval, Object sexp)
        throws FusionException
    {
        return ((BaseSexp) sexp).size();
    }


    static Object unsafePairDot(Evaluator eval, Object pair, int pos)
        throws FusionException
    {
        ImmutablePair p = (ImmutablePair) pair;
        for ( ; pos > 0; pos--)
        {
            Object tail = p.myTail;
            if (tail instanceof ImmutablePair)
            {
                p = (ImmutablePair) tail;
            }
            else
            {
                return voidValue(eval);
            }
        }
        return p.myHead;
    }


    static Object unsafePairHead(Evaluator eval, Object pair)
        throws FusionException
    {
        return ((ImmutablePair) pair).myHead;
    }


    static Object unsafePairTail(Evaluator eval, Object pair)
        throws FusionException
    {
        return ((ImmutablePair) pair).myTail;
    }


    static Object unsafeSexpAdd(Evaluator eval, Object sexp, Object value)
    {
        // TODO copy annotations?
        if (sexp instanceof NullSexp)
        {
            return new ImmutablePair(value, EMPTY_SEXP);
        }

        return new ImmutablePair(value, sexp);
    }

    static FusionIterator unsafeSexpIterator(Evaluator eval, Object sexp)
        throws FusionException
    {
        BaseSexp c = (BaseSexp) sexp;
        return new SexpIterator(c);
    }


    //========================================================================
    // Transformers


    /**
     * @param sexp must be a proper sexp; it is not type-checked!
     */
    static IonSexp unsafeCopyToIonSexp(Object sexp,
                                       ValueFactory factory,
                                       boolean throwOnConversionFailure)
        throws FusionException
    {
        BaseSexp base = (BaseSexp) sexp;
        return base.copyToIonValue(factory, throwOnConversionFailure);
    }

    /**
     * @param sexp must be a proper sexp; it is not type-checked!
     */
    static IonSexp unsafeCopyToIonSexp(Object sexp, ValueFactory factory)
        throws FusionException
    {
        BaseSexp base = (BaseSexp) sexp;
        return base.copyToIonValue(factory, true);
    }

    /**
     * @param sexp must be a proper sexp; it is not type-checked!
     *
     * @return null if the sexp and its contents cannot be ionized.
     */
    static IonSexp unsafeCopyToIonSexpMaybe(Object sexp,
                                            ValueFactory factory)
        throws FusionException
    {
        BaseSexp base = (BaseSexp) sexp;
        return base.copyToIonValue(factory, false);
    }


    //========================================================================


    static abstract class BaseSexp
        extends BaseSequence
    {
        BaseSexp() {}

        BaseSexp(String[] annotations)
        {
            super(annotations);
        }

        @Override
        int size() throws FusionException { return 0; }

        @Override
        Object dot(Evaluator eval, int pos)
            throws FusionException
        {
            return voidValue(eval);
        }

        @Override
        Object unsafeRef(Evaluator eval, int pos)
            throws FusionException
        {
            String message =
                "No index " + pos + " in sequence " + this;
            throw new IndexOutOfBoundsException(message);
        }

        @Override
        abstract IonSexp copyToIonValue(ValueFactory factory,
                                        boolean throwOnConversionFailure)
            throws FusionException;
    }


    private static final class NullSexp
        extends BaseSexp
    {
        private NullSexp() {}

        NullSexp(String[] annotations)
        {
            super(annotations);
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        IonSexp copyToIonValue(ValueFactory factory,
                              boolean throwOnConversionFailure)
            throws FusionException
        {
            IonSexp sexp = factory.newNullSexp();
            sexp.setTypeAnnotations(myAnnotations);
            return sexp;
        }

        @Override
        void write(Evaluator eval, Appendable out) throws IOException
        {
            writeAnnotations(out, myAnnotations);
            out.append("null.sexp");
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.writeNull(IonType.SEXP);
        }
    }


    private static final class EmptySexp
        extends BaseSexp
    {
        private EmptySexp() {}

        EmptySexp(String[] annotations)
        {
            super(annotations);
        }

        @Override
        IonSexp copyToIonValue(ValueFactory factory,
                              boolean throwOnConversionFailure)
            throws FusionException
        {
            IonSexp sexp = factory.newEmptySexp();
            sexp.setTypeAnnotations(myAnnotations);
            return sexp;
        }

        @Override
        void write(Evaluator eval, Appendable out) throws IOException
        {
            writeAnnotations(out, myAnnotations);
            out.append("()");
        }

        @Override
        public void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.stepIn(IonType.SEXP);
            out.stepOut();
        }
    }


    static final class ImmutablePair
        extends BaseSexp
    {
        private final Object myHead;
        private final Object myTail;

        private ImmutablePair(Object head, Object tail)
        {
            myHead = head;
            myTail = tail;
        }

        private ImmutablePair(String[] annotations, Object head, Object tail)
        {
            super(annotations);
            myHead = head;
            myTail = tail;
        }

        @Override
        int size()
            throws FusionException
        {
            int size = 1;
            ImmutablePair p = this;
            try
            {
                while (p.myTail instanceof ImmutablePair)
                {
                    p = (ImmutablePair) p.myTail;
                    size++;
                }
                return size;
            }
            catch (ClassCastException e)
            {
                throw new ArgTypeFailure("size", "proper sexp", 0, this);
            }
        }

        @Override
        Object dot(Evaluator eval, int pos)
            throws FusionException
        {
            ImmutablePair p = this;
            for ( ; pos > 0; pos--)
            {
                Object tail = p.myTail;
                if (! (tail instanceof ImmutablePair))
                {
                    return voidValue(eval);
                }
                p = (ImmutablePair) tail;
            }
            return p.myHead;
        }

        @Override
        Object unsafeRef(Evaluator eval, int i)
            throws FusionException
        {
            String message =
                "No index " + i + " in sequence " + this;
            throw new IndexOutOfBoundsException(message);
        }

        @Override
        IonSexp copyToIonValue(ValueFactory factory,
                              boolean throwOnConversionFailure)
            throws FusionException
        {
            IonSexp is = factory.newEmptySexp();
            is.setTypeAnnotations(myAnnotations);

            ImmutablePair pair = this;
            while (true)
            {
                IonValue ion = copyToIonValue(pair.myHead, factory,
                                              throwOnConversionFailure);
                if (ion == null) return null;

                is.add(ion);

                Object tail = pair.myTail;
                if (tail instanceof ImmutablePair)
                {
                    pair = (ImmutablePair) tail;
                }
                else if (tail instanceof EmptySexp)
                {
                    return is;
                }
                else if (throwOnConversionFailure)
                {
                    String message =
                        "Value is not convertable to Ion: " +
                        safeWriteToString(null, this);
                    throw new ContractFailure(message);
                }
                else
                {
                    return null; // failure
                }
            }
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            out.append('(');

            ImmutablePair pair = this;
            while (true)
            {
                if (pair != this) out.append(' ');

                dispatchWrite(eval, out, pair.myHead);

                Object tail = pair.myTail;
                if (tail instanceof ImmutablePair)
                {
                    pair = (ImmutablePair) tail;
                }
                else if (tail instanceof EmptySexp)
                {
                    break;
                }
                else
                {
                    out.append(" {.} ");
                    dispatchWrite(eval, out, tail);
                    break;
                }
            }

            out.append(')');
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.stepIn(IonType.SEXP);

            ImmutablePair pair = this;
            while (true)
            {
                dispatchIonize(eval, out, pair.myHead);

                Object tail = pair.myTail;
                if (tail instanceof ImmutablePair)
                {
                    pair = (ImmutablePair) tail;
                }
                else if (tail instanceof EmptySexp)
                {
                    break;
                }
                else
                {
                    throw new IonizeFailure(this);
                }
            }
            out.stepOut();
        }
    }


    //========================================================================


    static final class SexpProc
        extends Procedure
    {
        SexpProc()
        {
            //    "                                                                               |
            super("Makes a fresh, immutable sexp containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return immutableSexp(eval, args);
        }
    }


    static final class IsSexpProc
        extends Procedure1
    {
        IsSexpProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is a sexp, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean result = isSexp(eval, arg);
            return eval.newBool(result);
        }
    }


    static final class PairProc
        extends Procedure2
    {
        PairProc()
        {
            //    "                                                                               |
            super("Makes a fresh, immutable pair containing the given `head` and `tail`.",
                  "head", "tail");
        }

        @Override
        Object doApply(Evaluator eval, Object head, Object tail)
            throws FusionException
        {
            return pair(eval, head, tail);
        }
    }


    static final class IsPairProc
        extends Procedure1
    {
        IsPairProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a pair, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isPair(eval, value);
            return eval.newBool(result);
        }
    }


    static final class UnsafePairHeadProc
        extends Procedure1
    {
        UnsafePairHeadProc()
        {
            //    "                                                                               |
            super("Returns the head of `pair`.",
                  "pair");
        }

        @Override
        Object doApply(Evaluator eval, Object pair)
            throws FusionException
        {
            return unsafePairHead(eval, pair);
        }
    }


    static final class UnsafePairTailProc
        extends Procedure1
    {
        UnsafePairTailProc()
        {
            //    "                                                                               |
            super("Returns the tail of `pair`.",
                  "pair");
        }

        @Override
        Object doApply(Evaluator eval, Object pair)
            throws FusionException
        {
            return unsafePairTail(eval, pair);
        }
    }


    private static final class SexpIterator
        extends AbstractIterator
    {
        private BaseSexp mySexp;

        public SexpIterator(BaseSexp sexp)
        {
            mySexp = sexp;
        }

        @Override
        boolean hasNext(Evaluator eval)
            throws FusionException
        {
            return (mySexp instanceof ImmutablePair);
        }

        @Override
        Object next(Evaluator eval)
            throws FusionException
        {
            try
            {
                ImmutablePair pair = (ImmutablePair) mySexp;
                mySexp = (BaseSexp) pair.myTail;
                return pair.myHead;
            }
            catch (ClassCastException e)
            {
                throw new ArgTypeFailure("iterator_next", "proper sexp",
                                         0, this);
            }
        }
    }
}
