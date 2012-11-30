// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.dispatchIonize;
import static com.amazon.fusion.FusionPrint.dispatchWrite;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.ion.util.IonTextUtils.printSymbol;
import static java.util.Collections.EMPTY_MAP;
import com.amazon.fusion.FusionCollection.BaseCollection;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


final class FusionStruct
{
    // Go away you big meany!
    private FusionStruct() {}


    static final NullStruct      NULL_STRUCT  = new NullStruct();
    static final NonNullImmutableStruct EMPTY_STRUCT =
        new NonNullImmutableStruct(EMPTY_MAP, EMPTY_STRING_ARRAY);

    //========================================================================
    // Constructors

    static Object structFromIonStruct(Evaluator eval, IonStruct struct)
    {
        String[] annotations = struct.getTypeAnnotations();
        // TODO FUSION-47 intern annotation text

        if (struct.isNullValue())
        {
            return nullStruct(eval, annotations);
        }

        Map<String, Object> map;
        if (struct.size() == 0)
        {
            map = Collections.emptyMap();
        }
        else
        {
            map = new HashMap<String, Object>(struct.size());
            for (IonValue v : struct)
            {
                String field  = v.getFieldName();

                // TODO be lazy about this injection
                Object newElt = eval.inject(v);
                structImplAdd(map, field, newElt);
            }
        }

        return immutableStruct(map, annotations);

    }


    static NullStruct nullStruct(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return NULL_STRUCT;
        }

        return new NullStruct(annotations);
    }


    static NonNullImmutableStruct immutableStruct(Map<String, Object> map)
    {
        if (map.size() == 0)
        {
            return EMPTY_STRUCT;
        }

        return new NonNullImmutableStruct(map, EMPTY_STRING_ARRAY);
    }

    static NonNullImmutableStruct immutableStruct(Map<String, Object> map,
                                                  String[] anns)
    {
        if (map.size() == 0)
        {
            if (anns.length == 0)
            {
                return EMPTY_STRUCT;
            }

            map = Collections.emptyMap(); // Just to normalize
        }

        return new NonNullImmutableStruct(map, anns);
    }


    static NonNullImmutableStruct immutableStruct(String[] names,
                                                  Object[] values,
                                                  String[] anns)
    {
        Map<String, Object> map;
        if (names.length == 0)
        {
            if (anns.length == 0)
            {
                return EMPTY_STRUCT;
            }

            map = Collections.emptyMap();
        }
        else
        {
            map = new HashMap<String, Object>(names.length);
            for (int i = 0; i < names.length; i++)
            {
                String field  = names[i];
                Object newElt = values[i];

                structImplAdd(map, field, newElt);
            }
        }

        return new NonNullImmutableStruct(map, anns);
    }


    static void structImplAdd(Map<String, Object> map, String name,
                              Object value)
    {
        Object prev = map.put(name, value);
        if (prev != null)
        {
            Object[] multi;
            if (prev instanceof Object[])
            {
                Object[] prevArray = (Object[]) prev;
                if (value instanceof Object[])
                {
                    Object[] moreArray = (Object[]) value;

                    int prevLen = prevArray.length;
                    int moreLen = moreArray.length;
                    multi = Arrays.copyOf(prevArray, prevLen + moreLen);
                    System.arraycopy(moreArray, 0, multi, prevLen, moreLen);
                }
                else
                {
                    multi = extend(prevArray, value);
                }
            }
            else if (value instanceof Object[])
            {
                multi = extend((Object[]) value, prev);
            }
            else
            {
                multi = new Object[] { prev, value };
            }
            map.put(name, multi);
        }
    }

    private static Object[] extend(Object[] array, Object element)
    {
        int len = array.length;
        array = Arrays.copyOf(array, len+1);
        array[len] = element;
        return array;
    }


    //========================================================================
    // Predicates


    static boolean isStruct(Evaluator eval, Object v)
    {
        return (v instanceof BaseStruct);
    }


    //========================================================================
    // Accessors

    static void unsafeStructFieldVisit(Evaluator eval, Object struct,
                                       StructFieldVisitor visitor)
        throws FusionException
    {
        ((BaseStruct) struct).visitFields(visitor);
    }

    /**
     * @param struct must be a {@link BaseStruct}.
     * @return void if the position is out of bounds.
     */
    static Object unsafeStructDot(Evaluator eval, Object struct, String field)
        throws FusionException
    {
        return ((BaseStruct) struct).dot(eval, field);
    }


    static Object unsafeStructRemoveKey(Evaluator eval, Object struct,
                                        String[] keys)
        throws FusionException
    {
        return ((BaseStruct) struct).removeKeys(keys);
    }


    static Object unsafeStructRetainKey(Evaluator eval, Object struct,
                                        String[] keys)
        throws FusionException
    {
        return ((BaseStruct) struct).retainKeys(keys);
    }

    static Object unsafeStructMerge(Evaluator eval, Object struct1,
                                    Object struct2)
        throws FusionException
    {
        return ((BaseStruct) struct1).merge((BaseStruct) struct2);
    }

    //========================================================================


    static interface StructFieldVisitor
    {
        Object visit(String name, Object value)
            throws FusionException;
    }


    static abstract class BaseStruct
        extends BaseCollection
    {
        BaseStruct() {}

        BaseStruct(String[] annotations)
        {
            super(annotations);
        }

        @Override
        abstract int size(); // Doesn't throw

        abstract void visitFields(StructFieldVisitor visitor)
            throws FusionException;

        // Return type isn't really right
        abstract ImmutableStruct transformFields(StructFieldVisitor visitor)
            throws FusionException;

        /** Returns void if the field doesn't exist. */
        abstract Object dot(Evaluator eval, String field)
            throws FusionException;

        abstract Object removeKeys(String[] keys)
            throws FusionException;

        abstract Object retainKeys(String[] keys)
            throws FusionException;

        abstract Object merge(BaseStruct other)
            throws FusionException;
    }


    static abstract class ImmutableStruct
        extends BaseStruct
    {
        ImmutableStruct() {}

        ImmutableStruct(String[] annotations)
        {
            super(annotations);
        }
    }


    static final class NullStruct
        extends ImmutableStruct
    {
        NullStruct() {}

        NullStruct(String[] annotations)
        {
            super(annotations);
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
        void visitFields(StructFieldVisitor visitor)
        {
        }

        @Override
        NullStruct transformFields(StructFieldVisitor visitor)
        {
            return this;
        }

        @Override
        Object dot(Evaluator eval, String field)
            throws FusionException
        {
            return voidValue(eval);
        }

        @Override
        Object removeKeys(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        Object retainKeys(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        Object merge(BaseStruct other)
            throws FusionException
        {
            if (other.size() == 0) return this;

            // We know it has children.
            NonNullImmutableStruct is = (NonNullImmutableStruct) other;
            return new NonNullImmutableStruct(is.myMap, myAnnotations);
        }

        @Override
        void write(Evaluator eval, Appendable out) throws IOException
        {
            writeAnnotations(out, myAnnotations);
            out.append("null.struct");
        }

        @Override
        public void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);
            out.writeNull(IonType.STRUCT);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException
        {
            IonStruct is = factory.newNullStruct();
            is.setTypeAnnotations(myAnnotations);
            return is;
        }
    }


    static final class NonNullImmutableStruct
        extends ImmutableStruct
    {
        /**
         * For repeated fields, the value is Object[] otherwise it's a
         * non-array Object.
         */
        private final Map<String, Object> myMap;

        /**
         * We can't use {@link #myMap}.size() because that doesn't count
         * repeated fields.
         */
        private final int mySize;

        /**
         * @param map must not be null.
         */
        private NonNullImmutableStruct(Map<String, Object> map, String[] anns)
        {
            super(anns);
            assert map != null;
            myMap = map;

            int size = 0;
            for (Object values : map.values())
            {
                if (values instanceof Object[])
                {
                    size += ((Object[]) values).length;
                }
                else
                {
                    size++;
                }

            }
            mySize = size;
        }

        Map<String, Object> copyMap()
        {
            return new HashMap<String, Object>(myMap);
        }

        @Override
        int size()
        {
            return mySize;
        }

        @Override
        void visitFields(StructFieldVisitor visitor)
            throws FusionException
        {
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    for (Object element : (Object[]) value)
                    {
                        visitor.visit(fieldName, element);
                    }
                }
                else
                {
                    visitor.visit(fieldName, value);
                }
            }
        }


        @Override
        NonNullImmutableStruct transformFields(StructFieldVisitor visitor)
            throws FusionException
        {
            if (mySize == 0) return this;

            boolean mustReplaceThis = false;

            // Make a copy of the map, then mutate it to replace children
            // as necessary.
            Map<String, Object> newMap = copyMap();

            for (Map.Entry<String, Object> entry : newMap.entrySet())
            {
                String fieldName = entry.getKey();

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    Object[] children = (Object[]) value;
                    int childCount = children.length;

                    boolean mustReplaceArray = false;
                    Object[] newChildren = new Object[childCount];
                    for (int i = 0; i < childCount; i++)
                    {
                        Object child = children[i];
                        Object newChild = visitor.visit(fieldName, child);
                        if (newChild != child)
                        {
                            mustReplaceArray = true;
                        }
                        newChildren[i] = newChild;
                    }

                    if (mustReplaceArray)
                    {
                        entry.setValue(newChildren);
                        mustReplaceThis = true;
                    }
                }
                else
                {
                    Object newChild = visitor.visit(fieldName, value);
                    if (newChild != value)
                    {
                        entry.setValue(newChild);
                        mustReplaceThis = true;
                    }
                }
            }

            if (! mustReplaceThis) return this;

            return new NonNullImmutableStruct(newMap, myAnnotations);
        }

        Object get(String fieldName)
        {
            Object result = myMap.get(fieldName);
            if (result == null) return null;
            if (result instanceof Object[])
            {
                return ((Object[]) result)[0];
            }

            return result;
        }

        @Override
        Object dot(Evaluator eval, String field)
            throws FusionException
        {
            Object result = myMap.get(field);
            if (result == null)
            {
                return voidValue(eval);
            }
            if (result instanceof Object[])
            {
                return ((Object[]) result)[0];
            }

            return result;
        }

        @Override
        Object removeKeys(String[] keys)
            throws FusionException
        {
            Map<String,Object> newMap = new HashMap<String,Object>(myMap);
            List<String> asList = Arrays.asList(keys);

            boolean modified = newMap.keySet().removeAll(asList);
            if (! modified) return this;

            if (newMap.isEmpty()) newMap = Collections.emptyMap();

            return new NonNullImmutableStruct(newMap, myAnnotations);
        }

        @Override
        Object retainKeys(String[] keys)
            throws FusionException
        {
            Map<String,Object> newMap = new HashMap<String,Object>(myMap);
            List<String> asList = Arrays.asList(keys);

            boolean modified = newMap.keySet().retainAll(asList);
            if (! modified) return this;

            if (newMap.isEmpty()) newMap = Collections.emptyMap();

            return new NonNullImmutableStruct(newMap, myAnnotations);
        }

        @Override
        Object merge(BaseStruct other)
            throws FusionException
        {
            if (other.size() == 0) return this;

            // We know it has children.
            NonNullImmutableStruct is = (NonNullImmutableStruct) other;

            Map<String,Object> newMap;

            if (myMap.size() == 0)
            {
                newMap = is.myMap;
            }
            else
            {
                newMap = new HashMap<String,Object>(myMap);
                for (Map.Entry<String,Object> entry : is.myMap.entrySet())
                {
                    String name  = entry.getKey();
                    Object value = entry.getValue();
                    structImplAdd(newMap, name, value);
                }
            }

            return new NonNullImmutableStruct(newMap, myAnnotations);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            out.append('{');
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    for (Object element : (Object[]) value)
                    {
                        printSymbol(out, fieldName);
                        out.append(':');
                        dispatchWrite(eval, out, element);
                        out.append(',');
                    }
                }
                else
                {
                    printSymbol(out, fieldName);
                    out.append(':');
                    dispatchWrite(eval, out, value);
                    out.append(',');
                }
            }
            out.append('}');
        }

        @Override
        public void ionize(Evaluator eval, IonWriter out)
            throws IOException, FusionException
        {
            out.setTypeAnnotations(myAnnotations);

            out.stepIn(IonType.STRUCT);
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();
                out.setFieldName(fieldName);

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    Object[] children = (Object[]) value;
                    for (Object child : children)
                    {
                        dispatchIonize(eval, out, child);
                    }
                }
                else
                {
                    dispatchIonize(eval, out, value);
                }
            }
            out.stepOut();
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException
        {
            IonStruct is = factory.newEmptyStruct();
            is.setTypeAnnotations(myAnnotations);

            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    Object[] children = (Object[]) value;
                    for (Object child : children)
                    {
                        IonValue ion = copyToIonValue(child, factory,
                                                      throwOnConversionFailure);
                        is.add(fieldName, ion);
                    }
                }
                else
                {
                    IonValue ion = copyToIonValue(value, factory,
                                                  throwOnConversionFailure);
                    is.add(fieldName, ion);
                }
            }

            return is;
        }
    }
}
