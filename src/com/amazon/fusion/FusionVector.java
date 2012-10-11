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


    static Object makeVectorFrom(Evaluator eval, List<Object> values)
    {
        Object[] v = new Object[values.size()];
        values.toArray(v);
        return new MutableVector(v);
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


    @Deprecated
    static Iterator<?> unsafeJavaIterate(Object vector)
    {
        return ((BaseVector) vector).javaIterate();
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


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static IonList unsafeCopyToIonList(Object vector, ValueFactory factory)
        throws FusionException
    {
        Object[] v = arrayFor(vector);
        int len = v.length;
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
        Object[] v = arrayFor(vector);
        int len = v.length;
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
        final Object[] myValues;

        BaseVector(Object[] values)
        {
            myValues = values;
        }


        abstract BaseVector makeSimilar(Object[] values);


        int size()
        {
            return myValues.length;
        }


        Object unsafeRef(int i)
        {
            return myValues[i];
        }

        Iterator<?> javaIterate()
        {
            return Arrays.asList(myValues).iterator();
        }


        BaseVector add(Object value)
        {
            int len = myValues.length;
            Object[] copy = Arrays.copyOf(myValues, len + 1);
            copy[len] = value;
            return makeSimilar(copy);
        }


        @Override
        void write(Appendable out) throws IOException
        {
            out.append('[');

            int length = myValues.length;
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
                for (int i = 0; i < myValues.length; i++)
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


    private static final class MutableVector
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



    //========================================================================


    static final class IsVectorProc
        extends Procedure1
    {
        IsVectorProc()
        {
            //    "                                                                               |
            super("Determines whether a VALUE is a vector, returning true or false.",
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
            super("Determines whether a VALUE is an immutable vector, returning true or false.",
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
            super("Determines whether a VALUE is an mutable vector, returning true or false.",
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


    static final class VectorProc
        extends Procedure
    {
        VectorProc()
        {
            //    "                                                                               |
            super("Makes a fresh, mutable vector containing the given VALUEs.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return FusionVector.makeVectorFrom(eval, args);
        }
    }


    static final class UnsafeVectorSizeProc
        extends Procedure1
    {
        UnsafeVectorSizeProc()
        {
            //    "                                                                               |
            super("Returns the size of VECTOR.",
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
            super("Returns the element of VECTOR at slot POS (zero-based).",
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
            super("Changes the element of VECTOR at slot POS (zero-based). This assumes that the\n" +
                  "VECTOR is mutable and that the POS is valid.",
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
            super("Returns a vector similar to VECTOR with the VALUE added to the end.",
                  "vector", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object vector, Object value)
            throws FusionException
        {
            return unsafeVectorAdd(eval, vector, value);
        }
    }
}
