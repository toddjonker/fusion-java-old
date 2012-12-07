// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVoid.voidValue;
import com.amazon.fusion.FusionSequence.BaseSequence;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
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

    static final ImmutableVector EMPTY_IMMUTABLE_VECTOR =
        new ImmutableVector(FusionUtils.EMPTY_OBJECT_ARRAY);

    static final NullVector NULL_VECTOR = new NullVector();

    //========================================================================
    // Constructors


    /**
     * Turns null.list into a {@link NullVector} with the same annotations.
     */
    static BaseVector vectorFromIonSequence(Evaluator eval, IonSequence seq)
    {
        String[] annotations = seq.getTypeAnnotations();
        // TODO FUSION-47 intern annotation text

        if (seq.isNullValue())
        {
            return nullVector(eval, annotations);
        }

        int size = seq.size();
        if (size == 0)
        {
            return immutableVector(eval, annotations, EMPTY_OBJECT_ARRAY);
        }
        else
        {
            Object[] elts = seq.toArray(new Object[size]);
            return new LazyInjectingVector(annotations, elts);
        }
    }


    static NullVector nullVector(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return NULL_VECTOR;
        }

        return new NullVector(annotations);
    }


    /**
     * Caller must have injected children.
     * @param elements must not be null. This method assumes ownership!
     */
    static Object vector(Evaluator eval, Object[] elements)
    {
        return new MutableVector(elements);
    }


    /**
     * Creates a mutable vector containing the values.
     * @param elements must be injected.
     */
    static <T> MutableVector vector(Evaluator eval, List<T> elements)
    {
        Object[] v = new Object[elements.size()];
        elements.toArray(v);
        return new MutableVector(v);
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static ImmutableVector immutableVector(Evaluator eval, Object[] elements)
    {
        if (elements.length == 0)
        {
            return EMPTY_IMMUTABLE_VECTOR;
        }
        else
        {
            return new ImmutableVector(elements);
        }
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static ImmutableVector immutableVector(Evaluator eval,
                                           String[] annotations,
                                           Object[] elements)
    {
        if (elements.length == 0 && annotations.length == 0)
        {
            return EMPTY_IMMUTABLE_VECTOR;
        }
        else
        {
            return new ImmutableVector(annotations, elements);
        }
    }


    /**
     * Caller must have injected children.
     * @param elements must not be null
     */
    static <T> ImmutableVector immutableVector(Evaluator eval,
                                               List<T> elements)
    {
        int size = elements.size();
        if (size == 0)
        {
            return EMPTY_IMMUTABLE_VECTOR;
        }
        else
        {
            Object[] elts = elements.toArray(new Object[size]);
            return new ImmutableVector(elts);
        }
    }


    /**
     * Caller must have injected children.
     * @param elements must not be null
     */
    static Object stretchyVector(Evaluator eval, Object[] elements)
    {
        return new StretchyVector(elements);
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

    static boolean isNullVector(Evaluator eval, Object v)
    {
        return (v instanceof NullVector);
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

    /**
     * @return not null.
     */
    static String[] unsafeVectorAnnotationStrings(Evaluator eval,
                                                  Object vector)
    {
        return ((BaseVector) vector).myAnnotations;
    }

    static int unsafeVectorSize(Evaluator eval, Object vector)
    {
        return ((BaseVector) vector).size();
    }


    static Object unsafeVectorRef(Evaluator eval, Object vector, int pos)
    {
        return ((BaseVector) vector).unsafeRef(eval, pos);
    }


    static void unsafeVectorSet(Evaluator eval, Object vector,
                                int pos, Object value)
    {
        ((MutableVector) vector).unsafeSet(pos, value);
    }

    static Object unsafeVectorAdd(Evaluator eval, Object vector, Object value)
    {
        return ((BaseVector) vector).add(eval, value);
    }

    static Object unsafeVectorAddM(Evaluator eval, Object vector, Object value)
    {
        return ((BaseVector) vector).addM(eval, value);
    }


    @Deprecated
    static Iterator<?> unsafeJavaIterate(Evaluator eval, Object vector)
    {
        return ((BaseVector) vector).javaIterate(eval);
    }

    static Object unsafeVectorIterate(Evaluator eval, Object vector)
    {
        return Iterators.iterate(unsafeJavaIterate(eval, vector));
    }


    //========================================================================
    // Transformers


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static void unsafeVectorCopy(Evaluator eval, Object vector, int srcPos,
                                 Object[] dest, int destPos, int length)
    {
        ((BaseVector) vector).unsafeCopy(eval, srcPos, dest, destPos, length);
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
            copy = EMPTY_OBJECT_ARRAY;
        }
        else
        {
            copy = new Object[length];
            unsafeVectorCopy(eval, vector, srcPos, copy, 0, length);
        }

        return ((BaseVector) vector).makeSimilar(EMPTY_STRING_ARRAY, copy);
    }


    static Object unsafeVectorConcatenateM(Evaluator eval, Object vector,
                                           Object[] args)
    {
        return ((BaseVector) vector).concatenateM(eval, args);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     */
    static IonList unsafeCopyToIonList(Object vector, ValueFactory factory)
        throws FusionException
    {
        return unsafeCopyToIonList(vector, factory, true);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     *
     * @return null if the vector and its contents cannot be ionized.
     */
    static IonList unsafeCopyToIonListMaybe(Object vector,
                                            ValueFactory factory)
        throws FusionException
    {
        return unsafeCopyToIonList(vector, factory, false);
    }


    /**
     * @param vector must be a vector; it is not type-checked!
     *
     * @return null if the vector and its contents cannot be ionized.
     */
    static IonList unsafeCopyToIonList(Object vector,
                                       ValueFactory factory,
                                       boolean throwOnConversionFailure)
        throws FusionException
    {
        BaseVector base = (BaseVector) vector;
        return base.copyToIonValue(factory, throwOnConversionFailure);
    }


    //========================================================================


    /**
     * Prevents mutation from Java code and is distinguishable from mutable
     * vectors.
     */
    private abstract static class BaseVector
        extends BaseSequence
    {
        Object[] myValues;

        BaseVector(Object[] values)
        {
            myValues = values;
        }

        BaseVector(String[] annotations, Object[] values)
        {
            super(annotations);
            myValues = values;
        }


        /** Takes ownership of the array, doesn't make a copy. */
        abstract BaseVector makeSimilar(String[] annotations, Object[] values);


        @Override
        int size()
        {
            return myValues.length;
        }


        @Override
        Object dot(Evaluator eval, int pos)
            throws FusionException
        {
            if (pos < 0 || size() <= pos) return voidValue(eval);
            return myValues[pos];
        }

        @Override
        Object unsafeRef(Evaluator eval, int pos)
        {
            return myValues[pos];
        }


        void unsafeCopy(Evaluator eval, int srcPos, Object[] dest, int destPos,
                        int length)
        {
            System.arraycopy(myValues, srcPos, dest, destPos, length);
        }

        /**
         * @return null if the vector and its contents cannot be ionized
         *  UNLESS throwOnConversionFailure
         */
        @Override
        IonList copyToIonValue(ValueFactory factory,
                               boolean throwOnConversionFailure)
            throws FusionException
        {
            int len = size();
            IonValue[] ions = new IonValue[len];
            for (int i = 0; i < len; i++)
            {
                IonValue ion = copyToIonValue(myValues[i], factory,
                                              throwOnConversionFailure);
                if (ion == null)
                {
                    assert !throwOnConversionFailure;
                    return null;
                }

                ions[i] = ion;
            }

            IonList list = factory.newList(ions);
            list.setTypeAnnotations(myAnnotations);
            return list;
        }

        BaseVector add(Evaluator eval, Object value)
        {
            int len = size();
            Object[] copy = Arrays.copyOf(myValues, len + 1);
            copy[len] = value;
            return makeSimilar(myAnnotations, copy);
        }

        BaseVector addM(Evaluator eval, Object value)
        {
            return add(eval, value);
        }

        BaseVector concatenateM(Evaluator eval, Object[] args)
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

            return makeSimilar(myAnnotations, copy);
        }


        Iterator<?> javaIterate(Evaluator eval)
        {
            return Arrays.asList(myValues).iterator();
        }

        @Override
        void write(Appendable out) throws IOException
        {
            writeAnnotations(out, myAnnotations);

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
                out.setTypeAnnotations(myAnnotations);
                out.stepIn(IonType.LIST);
                for (int i = 0; i < size(); i++)
                {
                    dispatchWrite(out, myValues[i]);
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

        MutableVector(String[] annotations, Object[] values)
        {
            super(annotations, values);
        }

        @Override
        BaseVector makeSimilar(String[] annotations, Object[] values)
        {
            return new MutableVector(annotations, values);
        }

        void unsafeSet(int pos, Object value)
        {
            myValues[pos] = value;
        }
    }


    private static class ImmutableVector
        extends BaseVector
    {
        ImmutableVector(Object[] values)
        {
            super(values);
        }

        ImmutableVector(String[] annotations, Object[] values)
        {
            super(annotations, values);
        }

        @Override
        BaseVector makeSimilar(String[] annotations, Object[] values)
        {
            return new ImmutableVector(annotations, values);
        }
    }


    private static final class NullVector
        extends ImmutableVector
    {
        NullVector()
        {
            super(EMPTY_OBJECT_ARRAY);
        }

        NullVector(String[] annotations)
        {
            super(annotations, EMPTY_OBJECT_ARRAY);
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        int size()
        {
            return 0;
        }

        @Override
        IonList copyToIonValue(ValueFactory factory,
                              boolean throwOnConversionFailure)
            throws FusionException
        {
            IonList list = factory.newNullList();
            list.setTypeAnnotations(myAnnotations);
            return list;
        }

        @Override
        void write(Appendable out) throws IOException
        {
            writeAnnotations(out, myAnnotations);
            out.append("null.list");
        }


        @Override
        public void write(IonWriter out)
            throws FusionException
        {
            try
            {
                out.setTypeAnnotations(myAnnotations);
                out.writeNull(IonType.LIST);
            }
            catch (IOException e)
            {
                throw new FusionException("I/O exception", e);
            }
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

        StretchyVector(String[] annotations, Object[] values)
        {
            super(annotations, values);
            mySize = values.length;
        }

        @Override
        BaseVector makeSimilar(String[] annotations, Object[] values)
        {
            return new StretchyVector(annotations, values);
        }

        @Override
        int size()
        {
            return mySize;
        }

        @Override
        BaseVector addM(Evaluator eval, Object value)
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
        BaseVector concatenateM(Evaluator eval, Object[] args)
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
        Iterator<?> javaIterate(Evaluator eval)
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

    private static final class LazyInjectingVector
        extends ImmutableVector
    {
        LazyInjectingVector(String[] annotations, Object[] values)
        {
            super(annotations, values);
            assert values.length != 0;
        }

        private void injectElements(Evaluator eval)
        {
            if (myValues[0] instanceof IonValue)
            {
                for (int i = 0; i < myValues.length; i++)
                {
                    myValues[i] = eval.inject((IonValue) myValues[i]);
                }
            }
        }

        @Override
        Object dot(Evaluator eval, int pos)
            throws FusionException
        {
            injectElements(eval);
            return super.dot(eval, pos);
        }

        @Override
        Object unsafeRef(Evaluator eval, int pos)
        {
            injectElements(eval);
            return myValues[pos];
        }

        @Override
        void unsafeCopy(Evaluator eval, int srcPos, Object[] dest, int destPos,
                        int length)
        {
            injectElements(eval);
            System.arraycopy(myValues, srcPos, dest, destPos, length);
        }


        @Override
        BaseVector add(Evaluator eval, Object value)
        {
            injectElements(eval);
            return super.add(eval, value);
        }

        @Override
        BaseVector concatenateM(Evaluator eval, Object[] args)
        {
            injectElements(eval);
            return super.concatenateM(eval, args);
        }

        @Override
        Iterator<?> javaIterate(Evaluator eval)
        {
            injectElements(eval);
            return super.javaIterate(eval);
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
            return immutableVector(eval, args);
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
            return vector(eval, args);
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
            super("Returns the number of elements in `vector`.",
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
