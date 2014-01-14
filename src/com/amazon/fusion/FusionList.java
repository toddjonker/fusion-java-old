// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionCompare.EqualityTier.LOOSE_EQUAL;
import static com.amazon.fusion.FusionCompare.EqualityTier.STRICT_EQUAL;
import static com.amazon.fusion.FusionCompare.EqualityTier.TIGHT_EQUAL;
import static com.amazon.fusion.FusionIo.dispatchIonize;
import static com.amazon.fusion.FusionIo.dispatchWrite;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionNumber.unsafeTruncateIntToJavaInt;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.Syntax.datumToStrippedSyntaxMaybe;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionCompare.EqualityTier;
import com.amazon.fusion.FusionSequence.BaseSequence;
import com.amazon.fusion.FusionSexp.BaseSexp;
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


final class FusionList
{
    private FusionList() {}

    static final ImmutableList EMPTY_IMMUTABLE_LIST =
        new ImmutableList(FusionUtils.EMPTY_OBJECT_ARRAY);

    static final NullList NULL_LIST = new NullList();

    //========================================================================
    // Constructors


    /**
     * Turns null.list into a {@link NullList} with the same annotations.
     */
    static BaseList listFromIonSequence(Evaluator eval, IonSequence seq)
    {
        String[] annotations = seq.getTypeAnnotations();
        // TODO FUSION-47 intern annotation text

        if (seq.isNullValue())
        {
            return nullList(eval, annotations);
        }

        int size = seq.size();
        if (size == 0)
        {
            return immutableList(eval, annotations, EMPTY_OBJECT_ARRAY);
        }
        else
        {
            Object[] elts = seq.toArray(new Object[size]);
            return new LazyInjectingList(annotations, elts);
        }
    }


    static NullList nullList(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return NULL_LIST;
        }

        return new NullList(annotations);
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null. This method assumes ownership!
     */
    static Object mutableList(Evaluator eval, Object[] elements)
    {
        return new MutableList(elements);
    }


    /**
     * Creates a mutable list containing the elements.
     * @param elements must be injected.
     */
    static <T> MutableList mutableList(Evaluator eval, List<T> elements)
    {
        Object[] v = new Object[elements.size()];
        elements.toArray(v);
        return new MutableList(v);
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static ImmutableList immutableList(Evaluator eval, Object[] elements)
    {
        if (elements.length == 0)
        {
            return EMPTY_IMMUTABLE_LIST;
        }
        else
        {
            return new ImmutableList(elements);
        }
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static ImmutableList immutableList(Evaluator eval,
                                       String[] annotations,
                                       Object[] elements)
    {
        if (elements.length == 0 && annotations.length == 0)
        {
            return EMPTY_IMMUTABLE_LIST;
        }
        else
        {
            return new ImmutableList(annotations, elements);
        }
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static <T> ImmutableList immutableList(Evaluator eval, List<T> elements)
    {
        int size = elements.size();
        if (size == 0)
        {
            return EMPTY_IMMUTABLE_LIST;
        }
        else
        {
            Object[] elts = elements.toArray(new Object[size]);
            return new ImmutableList(elts);
        }
    }


    /**
     * Caller must have injected elements.
     * @param elements must not be null
     */
    static Object stretchyList(Evaluator eval, Object[] elements)
    {
        return new StretchyList(elements);
    }


    //========================================================================
    // Predicates


    static boolean isList(Evaluator eval, Object v)
    {
        return (v instanceof BaseList);
    }

    static boolean isImmutableList(Evaluator eval, Object v)
    {
        return (v instanceof ImmutableList);
    }

    static boolean isNullList(Evaluator eval, Object v)
    {
        return (v instanceof NullList);
    }

    static boolean isMutableList(Evaluator eval, Object v)
    {
        return (v instanceof MutableList);
    }

    static boolean isStretchyList(Evaluator eval, Object v)
    {
        return (v instanceof StretchyList);
    }


    //========================================================================
    // Accessors

    /**
     * @return not null.
     */
    static String[] unsafeListAnnotationStrings(Evaluator eval, Object list)
    {
        return ((BaseList) list).myAnnotations;
    }

    static int unsafeListSize(Evaluator eval, Object list)
    {
        return ((BaseList) list).size();
    }


    static Object unsafeListRef(Evaluator eval, Object list, int pos)
    {
        return ((BaseList) list).unsafeRef(eval, pos);
    }


    static void unsafeListSet(Evaluator eval, Object list,
                                int pos, Object value)
    {
        ((MutableList) list).unsafeSet(pos, value);
    }

    static Object unsafeListAdd(Evaluator eval, Object list, Object value)
    {
        return ((BaseList) list).add(eval, value);
    }

    static Object unsafeListAddM(Evaluator eval, Object list, Object value)
    {
        return ((BaseList) list).addM(eval, value);
    }


    @Deprecated
    static Iterator<?> unsafeJavaIterate(Evaluator eval, Object list)
    {
        return ((BaseList) list).javaIterate(eval);
    }

    static FusionIterator unsafeListIterator(Evaluator eval, Object list)
    {
        return FusionIterator.iterate(eval, unsafeJavaIterate(eval, list));
    }


    //========================================================================
    // Transformers


    /**
     * @param list must be a list; it is not type-checked!
     */
    static void unsafeListCopy(Evaluator eval, Object list, int srcPos,
                               Object[] dest, int destPos, int length)
    {
        ((BaseList) list).unsafeCopy(eval, srcPos, dest, destPos, length);
    }


    /**
     * Copies the elements of this list into a new array.
     *
     * @param list must be a list; it is not type-checked!
     *
     * @return null if this is null.list.
     */
    static Object[] unsafeListExtract(Evaluator eval, Object list)
    {
        return ((BaseList) list).extract(eval);
    }


    /**
     * @param list must be a list; it is not type-checked!
     */
    static BaseList unsafeListSubseq(Evaluator eval, Object list,
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
            unsafeListCopy(eval, list, srcPos, copy, 0, length);
        }

        return ((BaseList) list).makeSimilar(EMPTY_STRING_ARRAY, copy);
    }


    static Object unsafeListAppendM(Evaluator eval, Object list, Object[] args)
    {
        return ((BaseList) list).appendM(eval, args);
    }


    static BaseSexp unsafeListToSexp(Evaluator eval, Object list)
        throws FusionException
    {
        if (list instanceof NullList)
        {
            return FusionSexp.nullSexp(eval, null); // TODO annotations
        }

        BaseSexp s = FusionSexp.EMPTY_SEXP;

        BaseList l = (BaseList) list;
        for (int i = l.size(); i-- != 0; )
        {
            Object head = l.elt(eval, i);
            s = FusionSexp.pair(eval, head, s);
        }

        return s;
    }

    /**
     * @param list must be a list; it is not type-checked!
     */
    static IonList unsafeCopyToIonList(Object list, ValueFactory factory)
        throws FusionException
    {
        return unsafeCopyToIonList(list, factory, true);
    }


    /**
     * @param list must be a list; it is not type-checked!
     *
     * @return null if the list and its elements cannot be ionized.
     */
    static IonList unsafeCopyToIonListMaybe(Object list,
                                            ValueFactory factory)
        throws FusionException
    {
        return unsafeCopyToIonList(list, factory, false);
    }


    /**
     * @param list must be a list; it is not type-checked!
     *
     * @return null if the list and its elements cannot be ionized.
     */
    static IonList unsafeCopyToIonList(Object list,
                                       ValueFactory factory,
                                       boolean throwOnConversionFailure)
        throws FusionException
    {
        BaseList base = (BaseList) list;
        return base.copyToIonValue(factory, throwOnConversionFailure);
    }


    //========================================================================


    /**
     * Prevents mutation from Java code and is distinguishable from mutable
     * lists.
     */
    abstract static class BaseList
        extends BaseSequence
    {
        /**
         * The elements within this list.
         *
         * <b>WARNING!</b> The {@link LazyInjectingList} subclass may mutate
         * elements of this list while still appearing immutable. Every method
         * that reads from this array MUST be overridden there and properly
         * synchronized.
         */
        Object[] myValues;

        BaseList(Object[] values)
        {
            myValues = values;
        }

        BaseList(String[] annotations, Object[] values)
        {
            super(annotations);
            myValues = values;
        }


        /**
         * @param annotations must not be null. This method assumes ownership
         * of the array and it must not be modified later.
         */
        abstract BaseList makeSimilar(String[] annotations, Object[] values);


        @Override
        int size()
        {
            return myValues.length;
        }


        @Override
        Object elt(Evaluator eval, int pos)
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

        Object[] extract(Evaluator eval)
        {
            int size = size();
            Object[] elements = new Object[size];
            unsafeCopy(eval, 0, elements, 0, size);
            return elements;
        }


        private static BaseBool actualListEqual(Evaluator    eval,
                                                EqualityTier tier,
                                                BaseList     left,
                                                BaseList     right)
            throws FusionException
        {
            assert ! (left.isAnyNull() || right.isAnyNull());

            int size = left.size();
            if (size == right.size())
            {
                Object[] lVals = left.myValues;
                Object[] rVals = right.myValues;

                for (int i = 0; i < size; i++)
                {
                    Object lv = lVals[i];
                    Object rv = rVals[i];

                    BaseBool b = tier.eval(eval, lv, rv);
                    if (b.isFalse()) return b;
                }

                return trueBool(eval);
            }
            return falseBool(eval);
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseList)
            {
                BaseList r = (BaseList) right;
                if (! r.isAnyNull())
                {
                    return actualListEqual(eval, STRICT_EQUAL, this, r);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseList)
            {
                BaseList r = (BaseList) right;
                if (! r.isAnyNull())
                {
                    return actualListEqual(eval, TIGHT_EQUAL, this, r);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseSequence)
            {
                BaseSequence r = (BaseSequence) right;
                return r.looseEquals2(eval, this);
            }
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseList left)
            throws FusionException
        {
            // Neither is null.list
            return actualListEqual(eval, LOOSE_EQUAL, left, this);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseSexp left)
            throws FusionException
        {
            // (= <BaseList> <BaseSexp>) is implemented the other way
            return left.looseEquals2(eval, this);
        }

        /**
         * TODO FUSION-242 This needs to do cycle detection.
         *
         * @return null if an element can't be converted into syntax.
         */
        @Override
        SyntaxValue toStrippedSyntaxMaybe(Evaluator eval)
            throws FusionException
        {
            int size = size();
            if (size == 0 && (this instanceof ImmutableList))
            {
                return SyntaxList.make(eval, null, this);
            }

            Object[] children = new Object[size];
            for (int i = 0; i < size; i++)
            {
                Object rawChild = unsafeRef(eval, i);
                Object child = datumToStrippedSyntaxMaybe(eval, rawChild);
                if (child == null)
                {
                    // Hit something that's not syntax-able
                    return null;
                }
                children[i] = child;
            }

            String[] anns = annotationsAsJavaStrings();
            Object list = immutableList(eval, anns, children);
            return SyntaxList.make(eval, null, list);
        }


        /**
         * @return null if the list and its elements cannot be ionized
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

        BaseList add(Evaluator eval, Object value)
        {
            int len = size();
            Object[] copy = Arrays.copyOf(myValues, len + 1);
            copy[len] = value;
            return makeSimilar(myAnnotations, copy);
        }

        BaseList addM(Evaluator eval, Object value)
        {
            return add(eval, value);
        }

        BaseList appendM(Evaluator eval, Object[] args)
        {
            int myLen = myValues.length;
            int newLen = myLen;
            for (int i = 0; i < args.length; i++)
            {
                newLen += ((BaseList) args[i]).size();
            }

            if (newLen == myLen) return this; // Nothing to append

            Object[] copy = Arrays.copyOf(myValues, newLen);

            int pos = myLen;
            for (Object arg : args)
            {
                BaseList v = (BaseList) arg;
                int argLen = v.size();

                v.unsafeCopy(eval, 0, copy, pos, argLen);
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
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);

            out.append('[');

            int length = size();
            for (int i = 0; i < length; i++)
            {
                if (i != 0) out.append(", ");
                dispatchWrite(eval, out, myValues[i]);
            }

            out.append(']');
        }


        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.stepIn(IonType.LIST);
            for (int i = 0; i < size(); i++)
            {
                dispatchIonize(eval, out, myValues[i]);
            }
            out.stepOut();
        }
    }


    private static class MutableList
        extends BaseList
    {
        MutableList(Object[] values)
        {
            super(values);
        }

        MutableList(String[] annotations, Object[] values)
        {
            super(annotations, values);
        }

        @Override
        BaseList makeSimilar(String[] annotations, Object[] values)
        {
            return new MutableList(annotations, values);
        }

        @Override
        Object annotate(Evaluator eval, String[] annotations)
        {
            // Since this instance is mutable, we cannot share the array.
            Object[] values = Arrays.copyOf(myValues, size());
            return makeSimilar(annotations, values);
        }

        void unsafeSet(int pos, Object value)
        {
            myValues[pos] = value;
        }
    }


    private static class ImmutableList
        extends BaseList
    {
        ImmutableList(Object[] values)
        {
            super(values);
        }

        ImmutableList(String[] annotations, Object[] values)
        {
            super(annotations, values);
        }

        @Override
        BaseList makeSimilar(String[] annotations, Object[] values)
        {
            return new ImmutableList(annotations, values);
        }

        @Override
        Object annotate(Evaluator eval, String[] annotations)
        {
            // Since this instance is immutable, we can share the array.
            return makeSimilar(annotations, myValues);
        }
    }


    private static final class NullList
        extends ImmutableList
    {
        NullList()
        {
            super(EMPTY_OBJECT_ARRAY);
        }

        NullList(String[] annotations)
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
        Object annotate(Evaluator eval, String[] annotations)
        {
            return new NullList(annotations);
        }

        @Override
        Object[] extract(Evaluator eval)
        {
            return null;
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof NullList);
            return makeBool(eval, b);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof NullList);
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseList left)
            throws FusionException
        {
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseSexp left)
            throws FusionException
        {
            return makeBool(eval, left.isAnyNull());
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
        void write(Evaluator eval, Appendable out)
            throws IOException
        {
            writeAnnotations(out, myAnnotations);
            out.append("null.list");
        }


        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.writeNull(IonType.LIST);
        }
    }


    private static class StretchyList
        extends MutableList
    {
        private int mySize;

        StretchyList(Object[] values)
        {
            super(values);
            mySize = values.length;
        }

        StretchyList(String[] annotations, Object[] values)
        {
            super(annotations, values);
            mySize = values.length;
        }

        @Override
        BaseList makeSimilar(String[] annotations, Object[] values)
        {
            return new StretchyList(annotations, values);
        }

        @Override
        int size()
        {
            return mySize;
        }

        @Override
        BaseList addM(Evaluator eval, Object value)
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
        BaseList appendM(Evaluator eval, Object[] args)
        {
            int newLen = mySize;
            for (Object arg : args)
            {
                newLen += ((BaseList) arg).size();
            }

            if (myValues.length < newLen)
            {
                myValues = Arrays.copyOf(myValues, newLen);
            }

            int pos = mySize;
            for (Object arg : args)
            {
                BaseList v = (BaseList) arg;
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

    private static final class LazyInjectingList
        extends ImmutableList
    {
        LazyInjectingList(String[] annotations, Object[] values)
        {
            super(annotations, values);
            assert values.length != 0;
        }

        /**
         * Synchronized so this immutable class is thread-safe for reads.
         */
        private synchronized void injectElements(Evaluator eval)
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
        Object annotate(Evaluator eval, String[] annotations)
        {
            // Since this instance is immutable, we can share the array AFTER
            // we inject the children.
            injectElements(eval);
            return super.annotate(eval, annotations);
        }

        @Override
        Object elt(Evaluator eval, int pos)
            throws FusionException
        {
            injectElements(eval);
            return super.elt(eval, pos);
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
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            injectElements(eval);
            return super.looseEquals(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseList left)
            throws FusionException
        {
            injectElements(eval);
            return super.looseEquals2(eval, left);
        }

        @Override
        IonList copyToIonValue(ValueFactory factory,
                               boolean throwOnConversionFailure)
            throws FusionException
        {
            synchronized (this)
            {
                if (myValues[0] instanceof IonValue)
                {
                    int len = size();
                    IonValue[] ions = new IonValue[len];

                    for (int i = 0; i < len; i++)
                    {
                        ions[i] = factory.clone((IonValue) myValues[i]);
                    }

                    IonList list = factory.newList(ions);
                    list.setTypeAnnotations(myAnnotations);
                    return list;
                }
            }
            // else our elements have already been injected, copy as normal.

            return super.copyToIonValue(factory, throwOnConversionFailure);
        }

        @Override
        BaseList add(Evaluator eval, Object value)
        {
            injectElements(eval);
            return super.add(eval, value);
        }

        @Override
        BaseList appendM(Evaluator eval, Object[] args)
        {
            injectElements(eval);
            return super.appendM(eval, args);
        }

        @Override
        Iterator<?> javaIterate(Evaluator eval)
        {
            injectElements(eval);
            return super.javaIterate(eval);
        }

        @Override
        synchronized // So another thread doesn't inject while we write.
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // No need to inject our elements, they'll be output identically.
            super.write(eval, out);
        }

        @Override
        synchronized // So another thread doesn't inject while we ionize.
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            // No need to inject our elements, they'll be output identically.
            super.ionize(eval, out);
        }
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return the Fusion list, not null.
     */
    static Object checkListArg(Evaluator eval,
                               Procedure who,
                               String    expectation,
                               int       argNum,
                               Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseList)
        {
            return arg;
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return the Fusion list, not null.
     */
    static Object checkNullableListArg(Evaluator eval,
                                       Procedure who,
                                       int       argNum,
                                       Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable list";
        return checkListArg(eval, who, expectation, argNum, args);
    }


    //========================================================================
    // Procedures


    static final class IsListProc
        extends Procedure1
    {
        IsListProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a list, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isList(eval, value);
            return makeBool(eval, result);
        }
    }


    static final class IsImmutableListProc
        extends Procedure1
    {
        IsImmutableListProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is an immutable list, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isImmutableList(eval, value);
            return makeBool(eval, result);
        }
    }


    static final class IsMutableListProc
        extends Procedure1
    {
        IsMutableListProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a mutable list, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isMutableList(eval, value);
            return makeBool(eval, result);
        }
    }


    static final class IsStretchyListProc
        extends Procedure1
    {
        IsStretchyListProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a stretchy list, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isStretchyList(eval, value);
            return makeBool(eval, result);
        }
    }


    static final class ImmutableListProc
        extends Procedure
    {
        ImmutableListProc()
        {
            //    "                                                                               |
            super("Makes a fresh, immutable list containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return immutableList(eval, args);
        }
    }


    static final class MutableListProc
        extends Procedure
    {
        MutableListProc()
        {
            //    "                                                                               |
            super("Makes a fresh, mutable list containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return mutableList(eval, args);
        }
    }


    static final class StretchyListProc
        extends Procedure
    {
        StretchyListProc()
        {
            //    "                                                                               |
            super("Makes a fresh, stretchy list containing the given `value`s.",
                  "value", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return stretchyList(eval, args);
        }
    }


    static final class UnsafeListSizeProc
        extends Procedure1
    {
        UnsafeListSizeProc()
        {
            //    "                                                                               |
            super("Returns the number of elements in `list`.",
                  "list");
        }

        @Override
        Object doApply(Evaluator eval, Object list)
            throws FusionException
        {
            int result = unsafeListSize(eval, list);
            return makeInt(eval, result);
        }
    }


    static final class UnsafeListElementProc
        extends Procedure
    {
        UnsafeListElementProc()
        {
            //    "                                                                               |
            super("Returns the element of `list` at (zero-based) position `pos`. The `pos` must\n"
                + "be a non-null int with a valid value.",
                  "list", "pos");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            int pos = unsafeTruncateIntToJavaInt(eval, args[1]);

            return unsafeListRef(eval, args[0], pos);
        }
    }


    static final class UnsafeListSetProc
        extends Procedure
    {
        UnsafeListSetProc()
        {
            //    "                                                                               |
            super("Changes the element of `list` at (zero-based) position `pos`. This assumes\n" +
                  "that the `list` is mutable and that the `pos` is valid.",
                  "list", "pos", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            int pos = unsafeTruncateIntToJavaInt(eval, args[1]);

            unsafeListSet(eval, args[0], pos, args[2]);

            return voidValue(eval);
        }
    }


    static final class UnsafeListAddProc
        extends Procedure2
    {
        UnsafeListAddProc()
        {
            //    "                                                                               |
            super("Returns a list similar to `list` with the `value` added to the end.",
                  "list", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object list, Object value)
            throws FusionException
        {
            return unsafeListAdd(eval, list, value);
        }
    }


    static final class UnsafeListAddMProc
        extends Procedure2
    {
        UnsafeListAddMProc()
        {
            //    "                                                                               |
            super("Returns a list similar to `list` with the `value` added to the end.  The\n" +
                  "result may share structure with the list, which may also be mutated.\n" +
                  "\n" +
                  "In particular, when given a stretchy list, the input is expanded to contain\n" +
                  "the given value, and the result is the `list` argument.",
                  "list", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object list, Object value)
            throws FusionException
        {
            return unsafeListAddM(eval, list, value);
        }
    }


    static final class UnsafeListIterateProc
        extends Procedure1
    {
        UnsafeListIterateProc()
        {
            //    "                                                                               |
            super("Returns an iterator over the content of `list`.",
                  "list");
        }

        @Override
        Object doApply(Evaluator eval, Object list)
            throws FusionException
        {
            return unsafeListIterator(eval, list);
        }
    }
}
