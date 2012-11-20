// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;


final class FusionVector
{
    private FusionVector() {}

    static final BaseVector EMPTY_IMMUTABLE_VECTOR =
        new ImmutableVector(FusionUtils.EMPTY_OBJECT_ARRAY);


    //========================================================================
    // Constructors


    /**
     * Caller must have injected children.
     */
    static Object makeVectorFrom(Evaluator eval, Object value)
    {
        return new MutableVector(new Object[] { value });
    }


    /**
     * Caller must have injected children.
     * @param values must not be null. This method assumes ownership!
     */
    static Object makeVectorFrom(Evaluator eval, Object[] values)
    {
        return new MutableVector(values);
    }


    /**
     * Caller must have injected children.
     * @param values must not be null
     */
    static Object makeImmutableVectorFrom(Evaluator eval, Object[] values)
    {
        return new ImmutableVector(values);
    }

    /**
     * Creates a mutable vector containing the values.
     * @param values must be injected.
     */
    static Object makeVectorFrom(Evaluator eval, List<Object> values)
    {
        Object[] v = new Object[values.size()];
        values.toArray(v);
        return new MutableVector(v);
    }


    /**
     * Caller must have injected children.
     * @param values must not be null
     */
    static Object stretchyVector(Evaluator eval, Object[] values)
    {
        return new StretchyVector(values);
    }


    //========================================================================
    // Predicates


    static boolean isVector(Evaluator eval, Object v)
    {
        return (v instanceof BaseVector);
    }

    @Deprecated
    static boolean isVector(Object v)
    {
        return (v instanceof BaseVector);
    }

    static boolean isImmutableVector(Evaluator eval, Object v)
    {
        return (v instanceof ImmutableVector);
    }

    static boolean isMutableVector(Evaluator eval, Object v)
    {
        return (v instanceof MutableVector);
    }

    static boolean isStretchyVector(Evaluator eval, Object v)
    {
        return (v instanceof StretchyVector);
    }


    //========================================================================
    // Accessors


    static int unsafeVectorSize(Evaluator eval, Object vector)
    {
        return ((BaseVector) vector).size();
    }


    static Object unsafeVectorRef(Evaluator eval, Object vector, int pos)
    {
        return ((BaseVector) vector).unsafeRef(pos);
    }


    static void unsafeVectorSet(Evaluator eval, Object vector,
                                int pos, Object value)
    {
        ((MutableVector) vector).unsafeSet(pos, value);
    }

    static Object unsafeVectorAdd(Evaluator eval, Object vector, Object value)
    {
        return ((BaseVector) vector).add(value);
    }

    static Object unsafeVectorAddM(Evaluator eval, Object vector, Object value)
    {
        return ((BaseVector) vector).addM(value);
    }


    @Deprecated
    static Iterator<?> unsafeJavaIterate(Object vector)
    {
        return ((BaseVector) vector).javaIterate();
    }

    static Object unsafeVectorIterate(Evaluator eval, Object vector)
    {
        return Iterators.iterate(unsafeJavaIterate(vector));
    }


    //========================================================================
    // Transformers


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    private static Object[] arrayFor(Object vector)
    {
        return ((BaseVector) vector).myValues;
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static void unsafeVectorCopy(Evaluator eval, Object vector, int srcPos,
                                 Object[] dest, int destPos, int length)
    {
        Object[] v = arrayFor(vector);
        System.arraycopy(v, srcPos, dest, destPos, length);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static BaseVector unsafeVectorSubseq(Evaluator eval, Object vector,
                                         int srcPos, int length)
    {
        Object[] copy;
        if (length == 0)
        {
            copy = FusionUtils.EMPTY_OBJECT_ARRAY;
        }
        else
        {
            copy = new Object[length];
            Object[] v = arrayFor(vector);
            System.arraycopy(v, srcPos, copy, 0, length);
        }

        return ((BaseVector) vector).makeSimilar(copy);
    }


    static Object unsafeVectorConcatenateM(Evaluator eval, Object vector,
                                           Object[] args)
    {
        return ((BaseVector) vector).concatenateM(args);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static IonList unsafeCopyToIonList(Object vector, ValueFactory factory)
        throws FusionException
    {
        BaseVector base = (BaseVector) vector;
        Object[] v = base.myValues;
        int len = base.size();
        IonValue[] ions = new IonValue[len];
        for (int i = 0; i < len; i++)
        {
            ions[i] = FusionValue.copyToIonValue(v[i], factory);
        }

        return factory.newList(ions);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     *
     * @return null if the vector and its contents cannot be ionized.
     */
    static IonList unsafeCopyToIonValueMaybe(Object vector,
                                             ValueFactory factory)
        throws FusionException
    {
        BaseVector base = (BaseVector) vector;
        Object[] v = base.myValues;
        int len = base.size();
        IonValue[] ions = new IonValue[len];
        for (int i = 0; i < len; i++)
        {
            IonValue ion = FusionValue.copyToIonValueMaybe(v[i], factory);
            if (ion == null) return null;

            ions[i] = ion;
        }

        return factory.newList(ions);
    }


    //========================================================================


    /**
     * Prevents mutation from Java code and is distinguishable from mutable
     * vectors.
     */
    private abstract static class BaseVector
        extends FusionValue
        implements Writeable
    {
        Object[] myValues;

        BaseVector(Object[] values)
        {
            myValues = values;
        }


        /** Takes ownership of the array, doesn't make a copy. */
        abstract BaseVector makeSimilar(Object[] values);


        int size()
        {
            return myValues.length;
        }


        final Object unsafeRef(int i)
        {
            return myValues[i];
        }


        BaseVector add(Object value)
        {
            int len = size();
            Object[] copy = Arrays.copyOf(myValues, len + 1);
            copy[len] = value;
            return makeSimilar(copy);
        }

        BaseVector addM(Object value)
        {
            return add(value);
        }

        BaseVector concatenateM(Object[] args)
        {
            int newLen = myValues.length;
            for (int i = 0; i < args.length; i++)
            {
                newLen += ((BaseVector) args[i]).size();
            }

            Object[] copy = Arrays.copyOf(myValues, newLen);

            int pos = myValues.length;
            for (Object arg : args)
            {
                BaseVector v = (BaseVector) arg;
                int argLen = v.size();

                System.arraycopy(v.myValues, 0, copy, pos, argLen);
                pos += argLen;
            }
            assert pos == newLen;

            return makeSimilar(copy);
        }


        Iterator<?> javaIterate()
        {
            return Arrays.asList(myValues).iterator();
        }

        @Override
        void write(Appendable out) throws IOException
        {
            out.append('[');

            int length = size();
            for (int i = 0; i < length; i++)
            {
                if (i != 0) out.append(", ");
                dispatchWrite(out, myValues[i]);
            }

            out.append(']');
        }


        @Override
        public void write(IonWriter out)
            throws FusionException
        {
            try
            {
                out.stepIn(IonType.LIST);
                for (int i = 0; i < size(); i++)
                {
                    FusionValue.write(out, myValues[i]);
                }
                out.stepOut();
            }
            catch (IOException e)
            {
                throw new FusionException("I/O exception", e);
            }
        }
    }


    private static class MutableVector
        extends BaseVector
    {
        MutableVector(Object[] values)
        {
            super(values);
        }

        @Override
        BaseVector makeSimilar(Object[] values)
        {
            return new MutableVector(values);
        }

        void unsafeSet(int pos, Object value)
        {
            myValues[pos] = value;
        }
    }


    private static final class ImmutableVector
        extends BaseVector
    {
        ImmutableVector(Object[] values)
        {
            super(values);
        }

        @Override
        BaseVector makeSimilar(Object[] values)
        {
            return new ImmutableVector(values);
        }
    }


    private static class StretchyVector
        extends MutableVector
    {
        private int mySize;

        StretchyVector(Object[] values)
        {
            super(values);
            mySize = values.length;
        }

        @Override
        BaseVector makeSimilar(Object[] values)
        {
            return new StretchyVector(values);
        }

        @Override
        int size()
        {
            return mySize;
        }

        @Override
        BaseVector addM(Object value)
        {
            if (mySize == myValues.length)
            {
                // This expansion math is what's used by ArrayList, so I'm
                // assuming that it works well in practice.
                int newLength = (mySize * 3) / 2 + 1;
                myValues = Arrays.copyOf(myValues, newLength);
            }

            myValues[mySize++] = value;
            return this;
        }

        @Override
        BaseVector concatenateM(Object[] args)
        {
            int newLen = mySize;
            for (Object arg : args)
            {
                newLen += ((BaseVector) arg).size();
            }

            if (myValues.length < newLen)
            {
                myValues = Arrays.copyOf(myValues, newLen);
            }

            int pos = mySize;
            for (Object arg : args)
            {
                BaseVector v = (BaseVector) arg;
                int argLen = v.size();

                System.arraycopy(v.myValues, 0, myValues, pos, argLen);
                pos += argLen;
            }
            assert pos == newLen;
            mySize = pos;

            return this;
        }

        @Override
        Iterator<?> javaIterate()
        {
            Iterator<Object> iterator = new Iterator<Object>()
            {
                private int i = 0;

                @Override
                public boolean hasNext()
                {
                    return (i < mySize);
                }

                @Override
                public Object next()
                {
                    if (i < mySize) return myValues[i++];
                    throw new NoSuchElementException();
                }

                @Override
                public void remove()
                {
                    throw new UnsupportedOperationException();
                }
            };
            return iterator;
        }
    }


    //========================================================================


    static final class IsVectorProc
        extends Procedure1
    {
        IsVectorProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a vector, returning true or false.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            boolean result = isVector(eval, vector);
            return eval.newBool(result);
        }
    }


    static final class IsImmutableVectorProc
        extends Procedure1
    {
        IsImmutableVectorProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is an immutable vector, returning true or false.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            boolean result = isImmutableVector(eval, vector);
            return eval.newBool(result);
        }
    }


    static final class IsMutableVectorProc
        extends Procedure1
    {
        IsMutableVectorProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a mutable vector, returning true or false.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            boolean result = isMutableVector(eval, vector);
            return eval.newBool(result);
        }
    }


    static final class IsStretchyVectorProc
        extends Procedure1
    {
        IsStretchyVectorProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a stretchy vector, returning true or false.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            boolean result = isStretchyVector(eval, vector);
            return eval.newBool(result);
        }
    }


    static final class ImmutableVectorProc
        extends Procedure
    {
        ImmutableVectorProc()
        {
            //    "                                                                               |
            super("Makes a fresh, immutable vector containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return makeImmutableVectorFrom(eval, args);
        }
    }


    static final class VectorProc
        extends Procedure
    {
        VectorProc()
        {
            //    "                                                                               |
            super("Makes a fresh, mutable vector containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return makeVectorFrom(eval, args);
        }
    }


    static final class StretchyVectorProc
        extends Procedure
    {
        StretchyVectorProc()
        {
            //    "                                                                               |
            super("Makes a fresh, stretchy vector containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return stretchyVector(eval, args);
        }
    }


    static final class UnsafeVectorSizeProc
        extends Procedure1
    {
        UnsafeVectorSizeProc()
        {
            //    "                                                                               |
            super("Returns the mySize of `vector`.",
                  "vector");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            int result = unsafeVectorSize(eval, vector);
            return eval.newInt(result);
        }
    }


    static final class UnsafeVectorRefProc
        extends Procedure2
    {
        UnsafeVectorRefProc()
        {
            //    "                                                                               |
            super("Returns the element of `vector` at (zero-based) position `pos`.",
                  "vector", "pos");
        }

        @Override
        Object doApply(Evaluator eval, Object vector, Object posArg)
            throws FusionException
        {
            int pos = ((IonInt) posArg).intValue();

            return unsafeVectorRef(eval, vector, pos);
        }
    }


    static final class UnsafeVectorSetProc
        extends Procedure
    {
        UnsafeVectorSetProc()
        {
            //    "                                                                               |
            super("Changes the element of `vector` at (zero-based) position `pos`. This assumes\n" +
                  "that the `vector` is mutable and that the `pos` is valid.",
                  "vector", "pos", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            BaseVector vector = (BaseVector) args[0];
            int pos = ((IonInt) args[1]).intValue();

            unsafeVectorSet(eval, vector, pos, args[2]);

            return null;
        }
    }


    static final class UnsafeVectorAddProc
        extends Procedure2
    {
        UnsafeVectorAddProc()
        {
            //    "                                                                               |
            super("Returns a vector similar to `vector` with the `value` added to the end.",
                  "vector", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector, Object value)
            throws FusionException
        {
            return unsafeVectorAdd(eval, vector, value);
        }
    }


    static final class UnsafeVectorAddMProc
        extends Procedure2
    {
        UnsafeVectorAddMProc()
        {
            //    "                                                                               |
            super("Returns a vector similar to `vector` with the `value` added to the end.  The\n" +
                  "result may share structure with the vector, which may also be mutated.\n" +
                  "\n" +
                  "In particular, when given a stretchy vector, the input is expanded to contain\n" +
                  "the given value, and the result is the `vector` argument.",
                  "vector", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector, Object value)
            throws FusionException
        {
            return unsafeVectorAddM(eval, vector, value);
        }
    }


    static final class UnsafeVectorIterateProc
        extends Procedure1
    {
        UnsafeVectorIterateProc()
        {
            //    "                                                                               |
            super("Returns an iterator over the content of `vector`.",
                  "vector");
        }

        @Override
        Object doApply(Evaluator eval, Object vector)
            throws FusionException
        {
            return unsafeVectorIterate(eval, vector);
        }
    }
}
