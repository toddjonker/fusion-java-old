// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.dispatchIonize;
import static com.amazon.fusion.FusionIo.dispatchWrite;
import static com.amazon.fusion.FusionIterator.iterate;
import static com.amazon.fusion.FusionList.unsafeJavaIterate;
import static com.amazon.fusion.FusionText.unsafeTextToString;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.ion.util.IonTextUtils.printSymbol;
import static java.util.Collections.EMPTY_MAP;
import com.amazon.fusion.FusionCollection.BaseCollection;
import com.amazon.fusion.FusionIterator.AbstractIterator;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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
            map = new HashMap<>(struct.size());
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
            map = new HashMap<>(names.length);
            for (int i = 0; i < names.length; i++)
            {
                String field  = names[i];
                Object newElt = values[i];

                structImplAdd(map, field, newElt);
            }
        }

        return new NonNullImmutableStruct(map, anns);
    }


    static MutableStruct mutableStruct(Map<String, Object> map)
    {
        return new MutableStruct(map, EMPTY_STRING_ARRAY);
    }


    static MutableStruct mutableStruct(String[] names,
                                       Object[] values,
                                       String[] anns)
    {
        Map<String, Object> map;
        if (names.length == 0)
        {
            map = new HashMap<>();
        }
        else
        {
            map = new HashMap<>(names.length);
            for (int i = 0; i < names.length; i++)
            {
                String field  = names[i];
                Object newElt = values[i];

                structImplAdd(map, field, newElt);
            }
        }

        return new MutableStruct(map, anns);
    }


    /**
     * @param value may be an array (for repeated fields)
     */
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

    private static void structImplMergeM(Map<String, Object> map1,
                                         Map<String, Object> map2)
    {
        for (Map.Entry<String,Object> entry : map2.entrySet())
        {
            String key   = entry.getKey();
            Object value = entry.getValue();
            structImplAdd(map1, key, value);
        }
    }


    /** Mutates a multimap so it has only a single mapping for each key. */
    private static void structImplOneifyM(Map<String, Object> map)
    {
        for (Map.Entry<String,Object> entry : map.entrySet())
        {
            Object value = entry.getValue();
            if (value instanceof Object[])
            {
                Object first = ((Object[])value)[0];
                entry.setValue(first);
            }
        }
    }

    private static
    Map<String, Object> structImplOneify(Map<String, Object> map)
    {
        for (Object value : map.values())
        {
            if (value instanceof Object[])
            {
                Map<String,Object> newMap = new HashMap<>(map);
                structImplOneifyM(newMap);
                return newMap;
            }
        }
        return map;
    }

    private static void structImplMerge1M(Map<String, Object> map1,
                                          Map<String, Object> map2)
    {
        for (Map.Entry<String,Object> entry : map2.entrySet())
        {
            String key   = entry.getKey();
            Object value = entry.getValue();
            if (value instanceof Object[])
            {
                value = ((Object[])value)[0];
            }
            map1.put(key, value);
        }
    }


    private static Object[] extend(Object[] array, Object element)
    {
        int len = array.length;
        array = Arrays.copyOf(array, len+1);
        array[len] = element;
        return array;
    }

    private static Object bounceDefaultResult(Evaluator eval, Object def)
        throws FusionException
    {
        if (def instanceof Procedure)
        {
            def = eval.bounceTailCall((Procedure) def);
        }
        return def;
    }


    //========================================================================
    // Predicates


    static boolean isStruct(Evaluator eval, Object v)
    {
        return (v instanceof BaseStruct);
    }

    static boolean isImmutableStruct(Evaluator eval, Object v)
    {
        return (v instanceof ImmutableStruct);
    }

    static boolean isMutableStruct(Evaluator eval, Object v)
    {
        return (v instanceof MutableStruct);
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
        return ((BaseStruct) struct).elt(eval, field);
    }


    //========================================================================
    // Element modification


    static Object unsafeStructPut(Evaluator eval, Object struct,
                                  String key, Object value)
        throws FusionException
    {
        return ((BaseStruct) struct).put(eval, key, value);
    }


    static Object unsafeStructPutM(Evaluator eval, Object struct,
                                   String key, Object value)
        throws FusionException
    {
        return ((BaseStruct) struct).putM(eval, key, value);
    }


    //========================================================================
    // Bulk modification

    static Object unsafeStructRemoveKeys(Evaluator eval, Object struct,
                                         String[] keys)
        throws FusionException
    {
        if (keys.length == 0) return struct;

        return ((BaseStruct) struct).removeKeys(keys);
    }


    static Object unsafeStructRemoveKeysM(Evaluator eval, Object struct,
                                        String[] keys)
        throws FusionException
    {
        if (keys.length == 0) return struct;

        return ((BaseStruct) struct).removeKeysM(keys);
    }


    static Object unsafeStructRetainKeys(Evaluator eval, Object struct,
                                         String[] keys)
        throws FusionException
    {
        return ((BaseStruct) struct).retainKeys(keys);
    }

    static Object unsafeStructRetainKeysM(Evaluator eval, Object struct,
                                          String[] keys)
        throws FusionException
    {
        return ((BaseStruct) struct).retainKeysM(keys);
    }


    static Object unsafeStructMerge(Evaluator eval, Object struct1,
                                    Object struct2)
        throws FusionException
    {
        return ((BaseStruct) struct1).merge((BaseStruct) struct2);
    }

    static Object unsafeStructMergeM(Evaluator eval, Object struct1,
                                     Object struct2)
        throws FusionException
    {
        return ((BaseStruct) struct1).mergeM((BaseStruct) struct2);
    }


    static Object unsafeStructMerge1(Evaluator eval, Object struct1,
                                     Object struct2)
        throws FusionException
    {
        return ((BaseStruct) struct1).merge1((BaseStruct) struct2);
    }

    static Object unsafeStructMerge1M(Evaluator eval, Object struct1,
                                      Object struct2)
        throws FusionException
    {
        return ((BaseStruct) struct1).merge1M((BaseStruct) struct2);
    }


    //========================================================================


    static interface StructFieldVisitor
    {
        Object visit(String name, Object value)
            throws FusionException;
    }


    static interface BaseStruct
    {
        int size(); // Doesn't throw

        /**
         * Visits each field in the struct, stopping as soon as the visitation
         * returns non-null.
         */
        void visitFields(StructFieldVisitor visitor)
            throws FusionException;

        // Return type isn't really right
        ImmutableStruct transformFields(StructFieldVisitor visitor)
            throws FusionException;

        boolean hasKey(Evaluator eval, String key)
            throws FusionException;

        /** Returns void if the field doesn't exist. */
        Object elt(Evaluator eval, String field)
            throws FusionException;

        /**
         * Finds the value for a given name.
         *
         * @param def the default result. If its a procedure, it's called with
         * no arguments to determine the result. Otherwise it's returned as-is.
         */
        Object ref(Evaluator eval, String name, Object def)
            throws FusionException;

        Object put(Evaluator eval, String key, Object value)
            throws FusionException;

        Object putM(Evaluator eval, String key, Object value)
            throws FusionException;

        Object removeKeys(String[] keys)
            throws FusionException;

        Object removeKeysM(String[] keys)
            throws FusionException;

        Object retainKeys(String[] keys)
            throws FusionException;

        Object retainKeysM(String[] keys)
            throws FusionException;

        Object merge(BaseStruct other)
            throws FusionException;

        Object mergeM(BaseStruct other)
            throws FusionException;

        Object merge1(BaseStruct other)
            throws FusionException;

        Object merge1M(BaseStruct other)
            throws FusionException;
    }


    static interface ImmutableStruct
        extends BaseStruct
    {
    }


    private static final class NullStruct
        extends BaseCollection
        implements ImmutableStruct
    {
        NullStruct() {}

        NullStruct(String[] annotations)
        {
            super(annotations);
        }

        @Override
        public boolean isAnyNull()
        {
            return true;
        }

        @Override
        public int size()
        {
            return 0;
        }

        @Override
        Object annotate(Evaluator eval, String[] annotations)
            throws FusionException
        {
            return new NullStruct(annotations);
        }

        @Override
        public void visitFields(StructFieldVisitor visitor)
        {
        }

        @Override
        public NullStruct transformFields(StructFieldVisitor visitor)
        {
            return this;
        }

        @Override
        public boolean hasKey(Evaluator eval, String key)
            throws FusionException
        {
            return false;
        }

        @Override
        public Object elt(Evaluator eval, String field)
            throws FusionException
        {
            return voidValue(eval);
        }

        @Override
        public Object ref(Evaluator eval, String name, Object def)
            throws FusionException
        {
            return bounceDefaultResult(eval, def);
        }

        @Override
        public Object put(Evaluator eval, String key, Object value)
            throws FusionException
        {
            Map<String,Object> map = new HashMap<>(1);
            map.put(key, value);
            return immutableStruct(map, myAnnotations);
        }

        @Override
        public Object putM(Evaluator eval, String key, Object value)
            throws FusionException
        {
            return put(eval, key, value);
        }

        @Override
        public Object removeKeys(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        public Object removeKeysM(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        public Object retainKeys(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        public Object retainKeysM(String[] keys)
            throws FusionException
        {
            return this;
        }

        @Override
        public Object merge(BaseStruct other)
            throws FusionException
        {
            if (other.size() == 0) return this;

            // We know it has children.
            MapBasedStruct is = (MapBasedStruct) other;
            return new NonNullImmutableStruct(is.myMap, myAnnotations);
        }

        @Override
        public Object mergeM(BaseStruct other)
            throws FusionException
        {
            return merge(other);
        }

        @Override
        public Object merge1(BaseStruct other)
            throws FusionException
        {
            if (other.size() == 0) return this;

            // We know `other` has children.
            MapBasedStruct is = (MapBasedStruct) other;
            Map<String,Object> map = structImplOneify(is.myMap);
            return new NonNullImmutableStruct(map, myAnnotations);
        }

        @Override
        public Object merge1M(BaseStruct other)
            throws FusionException
        {
            return merge1(other);
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


    private abstract static class MapBasedStruct
        extends BaseCollection
        implements BaseStruct
    {
        /**
         * For repeated fields, the value is Object[] otherwise it's a
         * non-array Object.
         */
        protected final Map<String, Object> myMap;

        /**
         * We can't use {@link #myMap}.size() because that doesn't count
         * repeated fields.
         */
        private final int mySize;

        /**
         * @param map must not be null.
         */
        private MapBasedStruct(Map<String, Object> map, String[] anns)
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

        abstract MapBasedStruct makeSimilar(Map<String, Object> map,
                                            String[] annotations);

        MapBasedStruct makeSimilar(Map<String, Object> map)
        {
            return makeSimilar(map, myAnnotations);
        }

        Map<String, Object> copyMap()
        {
            return new HashMap<>(myMap);
        }

        @Override
        public int size()
        {
            return mySize;
        }

        @Override
        public void visitFields(StructFieldVisitor visitor)
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
                        if (visitor.visit(fieldName, element) != null) return;
                    }
                }
                else
                {
                    if (visitor.visit(fieldName, value) != null) return;
                }
            }
        }


        @Override
        public NonNullImmutableStruct transformFields(StructFieldVisitor visitor)
            throws FusionException
        {
            boolean mustReplaceThis = (this instanceof MutableStruct);

            if (mySize == 0 && !mustReplaceThis)
            {
                return (NonNullImmutableStruct) this;
            }

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

            if (! mustReplaceThis)
            {
                return (NonNullImmutableStruct) this;
            }

            return new NonNullImmutableStruct(newMap, myAnnotations);
        }

        @Override
        public boolean hasKey(Evaluator eval, String key)
            throws FusionException
        {
            return myMap.get(key) != null;
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
        public Object elt(Evaluator eval, String field)
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
        public Object ref(Evaluator eval, String name, Object def)
            throws FusionException
        {
            Object result = myMap.get(name);
            if (result == null)
            {
                return bounceDefaultResult(eval, def);
            }
            if (result instanceof Object[])
            {
                return ((Object[]) result)[0];
            }

            return result;
        }

        @Override
        public Object put(Evaluator eval, String key, Object value)
            throws FusionException
        {
            Map<String,Object> newMap = new HashMap<>(myMap);
            newMap.put(key, value);
            return makeSimilar(newMap);
        }

        @Override
        public Object removeKeys(String[] keys)
            throws FusionException
        {
            Map<String,Object> newMap = new HashMap<>(myMap);
            List<String> asList = Arrays.asList(keys);

            boolean modified = newMap.keySet().removeAll(asList);
            if (! modified) return this;

            return makeSimilar(newMap);
        }

        @Override
        public Object retainKeys(String[] keys)
            throws FusionException
        {
            Map<String,Object> newMap = new HashMap<>(myMap);
            List<String> asList = Arrays.asList(keys);

            boolean modified = newMap.keySet().retainAll(asList);
            if (! modified) return this;

            return makeSimilar(newMap);
        }

        @Override
        public Object merge(BaseStruct other)
            throws FusionException
        {
            if (other.size() == 0) return this;

            // We know it has children.
            MapBasedStruct is = (MapBasedStruct) other;

            Map<String,Object> newMap = new HashMap<>(myMap);
            structImplMergeM(newMap, is.myMap);

            return makeSimilar(newMap);
        }

        @Override
        public Object merge1(BaseStruct other)
            throws FusionException
        {
            Map<String,Object> newMap = structImplOneify(myMap);

            if (other.size() == 0)
            {
                if (newMap == myMap) return this;
            }
            else  // Other is not empty, we are going to make a change
            {
                // Copy our map if we haven't done so already while oneifying
                if (newMap == myMap)
                {
                    newMap = new HashMap<>(myMap);
                }

                MapBasedStruct is = (MapBasedStruct) other;
                structImplMerge1M(newMap, is.myMap);
            }

            return makeSimilar(newMap);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            out.append('{');

            boolean comma = false;
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    for (Object element : (Object[]) value)
                    {
                        if (comma) out.append(',');

                        printSymbol(out, fieldName);
                        out.append(':');
                        dispatchWrite(eval, out, element);
                        comma = true;
                    }
                }
                else
                {
                    if (comma) out.append(',');

                    printSymbol(out, fieldName);
                    out.append(':');
                    dispatchWrite(eval, out, value);
                    comma = true;
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

                Object value = entry.getValue();
                if (value instanceof Object[])
                {
                    Object[] children = (Object[]) value;
                    for (Object child : children)
                    {
                        out.setFieldName(fieldName);
                        dispatchIonize(eval, out, child);
                    }
                }
                else
                {
                    out.setFieldName(fieldName);
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
                        if (ion == null) return null;
                        is.add(fieldName, ion);
                    }
                }
                else
                {
                    IonValue ion = copyToIonValue(value, factory,
                                                  throwOnConversionFailure);
                    if (ion == null) return null;
                    is.add(fieldName, ion);
                }
            }

            return is;
        }
    }


    static final class NonNullImmutableStruct
        extends MapBasedStruct
        implements ImmutableStruct
    {
        /**
         * @param map
         * @param annotations
         */
        public NonNullImmutableStruct(Map<String, Object> map,
                                      String[] annotations)
        {
            super(map, annotations);
        }

        @Override
        MapBasedStruct makeSimilar(Map<String, Object> map,
                                   String[] annotations)
        {
            return immutableStruct(map, annotations);
        }

        @Override
        Object annotate(Evaluator eval, String[] annotations)
            throws FusionException
        {
            return makeSimilar(myMap, annotations);
        }

        @Override
        public Object putM(Evaluator eval, String key, Object value)
            throws FusionException
        {
            return put(eval, key, value);
        }

        @Override
        public Object removeKeysM(String[] keys)
            throws FusionException
        {
            return removeKeys(keys);
        }

        @Override
        public Object retainKeysM(String[] keys)
            throws FusionException
        {
            return retainKeys(keys);
        }

        @Override
        public Object mergeM(BaseStruct other)
            throws FusionException
        {
            return merge(other);
        }

        @Override
        public Object merge1M(BaseStruct other)
            throws FusionException
        {
            return merge1(other);
        }
    }


    private static final class MutableStruct
        extends MapBasedStruct
    {
        public MutableStruct(Map<String, Object> map,
                             String[] annotations)
        {
            super(map, annotations);
        }

        @Override
        MapBasedStruct makeSimilar(Map<String, Object> map,
                                   String[] annotations)
        {
            return new MutableStruct(map, annotations);
        }

        @Override
        Object annotate(Evaluator eval, String[] annotations)
            throws FusionException
        {
            return makeSimilar(copyMap(), annotations);
        }

        @Override
        public Object putM(Evaluator eval, String key, Object value)
            throws FusionException
        {
            myMap.put(key, value);
            return this;
        }

        @Override
        public Object removeKeysM(String[] keys)
            throws FusionException
        {
            List<String> asList = Arrays.asList(keys);
            myMap.keySet().removeAll(asList);
            return this;
        }

        @Override
        public Object retainKeysM(String[] keys)
            throws FusionException
        {
            List<String> asList = Arrays.asList(keys);
            myMap.keySet().retainAll(asList);
            return this;
        }

        @Override
        public Object mergeM(BaseStruct other) throws FusionException
        {
            if (other.size() != 0)
            {
                MapBasedStruct is = (MapBasedStruct) other;
                structImplMergeM(myMap, is.myMap);
            }

            return this;
        }

        @Override
        public Object merge1M(BaseStruct other) throws FusionException
        {
            // Remove any existing repeated fields.
            structImplOneifyM(myMap);

            if (other.size() != 0)
            {
                MapBasedStruct is = (MapBasedStruct) other;
                structImplMerge1M(myMap, is.myMap);
            }

            return this;
        }
    }


    //========================================================================


    static final class IsStructProc
        extends Procedure1
    {
        IsStructProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a struct, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean result = isStruct(eval, arg);
            return eval.newBool(result);
        }
    }



    static final class IsImmutableStructProc
        extends Procedure1
    {
        IsImmutableStructProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is an immutable struct, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isImmutableStruct(eval, value);
            return eval.newBool(result);
        }
    }



    static final class IsMutableStructProc
        extends Procedure1
    {
        IsMutableStructProc()
        {
            //    "                                                                               |
            super("Determines whether `value` is a mutable struct, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object value)
            throws FusionException
        {
            boolean result = isMutableStruct(eval, value);
            return eval.newBool(result);
        }
    }



    static final class UnsafeStructHasKeyProc
        extends Procedure
    {
        UnsafeStructHasKeyProc()
        {
            //    "                                                                               |
            super("UNSUPPORTED.  `name` must be text.",
                  "struct", "name");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            BaseStruct s = (BaseStruct) args[0];
            String key = unsafeTextToString(eval, args[1]);
            return eval.newBool(s.hasKey(eval, key));
        }
    }



    static final class UnsafeStructRefProc
        extends Procedure
    {
        UnsafeStructRefProc()
        {
            //    "                                                                               |
            super("Returns the value associated with `name` in `struct`. The `name` must be a\n" +
                  "non-null string or symbol.  If no field exists with the name, the `default` is\n" +
                  "used: if it is a procedure, it's applied to no arguments and the result is\n" +
                  "returned, otherwise the `default` is returned as-is.",
                  "struct", "name", "default");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            BaseStruct s = (BaseStruct) args[0];
            String name = unsafeTextToString(eval, args[1]);
            return s.ref(eval, name, args[2]);
        }
    }



    static final class UnsafeStructPutProc
        extends Procedure
    {
        UnsafeStructPutProc()
        {
            //    "                                                                               |
            super("*UNSUPPORTED!*",
                  "struct", "key", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            String key = unsafeTextToString(eval, args[1]);
            return unsafeStructPut(eval, args[0], key, args[2]);
        }
    }



    static final class UnsafeStructPutMProc
        extends Procedure
    {
        UnsafeStructPutMProc()
        {
            //    "                                                                               |
            super("*UNSUPPORTED!*",
                  "struct", "key", "value");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            String key = unsafeTextToString(eval, args[1]);
            return unsafeStructPutM(eval, args[0], key, args[2]);
        }
    }



    private static final class StructIterator
        extends AbstractIterator
    {
        private final Iterator<Map.Entry<String,Object>> myEntryIterator;
        private Iterator<Object>                         myMultiIterator;
        private Object                                   myCurrentKey;

        private StructIterator(MapBasedStruct struct)
        {
            myEntryIterator = struct.myMap.entrySet().iterator();
        }

        @Override
        boolean hasNext(Evaluator eval)
            throws FusionException
        {
            if (myMultiIterator != null)
            {
                if (myMultiIterator.hasNext())
                {
                    return true;
                }
                myMultiIterator = null;
            }
            return myEntryIterator.hasNext();
        }

        @Override
        Object next(Evaluator eval)
            throws FusionException
        {
            Object value;
            if (myMultiIterator != null)
            {
                value = myMultiIterator.next();
            }
            else
            {
                Map.Entry<String, Object> entry = myEntryIterator.next();
                myCurrentKey = eval.newSymbol(entry.getKey());

                value = entry.getValue();
                if (value instanceof Object[])
                {
                    Object[] vals = (Object[]) value;
                    myMultiIterator = Arrays.asList(vals).iterator();

                    // Safe since we have at least 1 element in the array:
                    value = myMultiIterator.next();
                }
            }

            // TODO route multi-values through the evaluator
            return new Object[] { myCurrentKey, value };
        }
    }

    static final class UnsafeStructIteratorProc
        extends Procedure1
    {
        UnsafeStructIteratorProc()
        {
            //    "                                                                               |
            super("Returns an iterator over the content of `struct`. Calls to `iterator_next` will\n" +
                  "return two results representing a single field: the field's name (as a symbol)\n" +
                  "and the field's value.",
                  "struct");
        }

        @Override
        Object doApply(final Evaluator eval, Object struct)
            throws FusionException
        {
            BaseStruct s = (BaseStruct) struct;
            if (s.size() == 0)
            {
                return iterate(eval, Arrays.asList().iterator());
            }

            return new StructIterator((MapBasedStruct) s);
        }
    }



    abstract static class BaseStructProc
        extends Procedure
    {
        BaseStructProc(String doc, String... argNames)
        {
            super(doc, argNames);
        }

        void checkArityEven(Object... args)
            throws FusionException
        {
            if ((args.length % 2) == 1)
            {
                String message =
                    "Expected even number of args, observed " + args.length;
                throw contractFailure(message);
            }
        }

        abstract Object makeIt(String[] names, Object[] values)
            throws FusionException;

        @Override
        final Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityEven(args);

            int fieldCount = (args.length / 2);
            String[] names  = new String[fieldCount];
            Object[] values = new Object[fieldCount];

            int fieldPos = 0;
            for (int i = 0; i < args.length; i++, fieldPos++)
            {
                names [fieldPos] = checkTextArg(i, args);
                values[fieldPos] = args[++i];
            }
            assert fieldPos == fieldCount;

            return makeIt(names, values);
        }
    }



    static final class StructProc
        extends BaseStructProc
    {
        StructProc()
        {
            //    "                                                                               |
            super("Constructs an immutable, non-null struct from alternating strings and values.\n" +
                  "Each `name` is a non-empty string or symbol denoting a field name, and the\n" +
                  "following `value` is the field's value.  Names may be repeated to produce\n" +
                  "repeated (multi-mapped) fields.",
                  "name", "value", DOTDOTDOT, DOTDOTDOT);
        }

        @Override
        Object makeIt(String[] names, Object[] values)
            throws FusionException
        {
            return immutableStruct(names, values, EMPTY_STRING_ARRAY);
        }
    }



    static final class MutableStructProc
        extends BaseStructProc
    {
        MutableStructProc()
        {
            //    "                                                                               |
            super("Constructs a mutable, non-null struct from alternating strings and values.\n" +
                  "Each `name` is a non-empty string or symbol denoting a field name, and the\n" +
                  "following `value` is the field's value.  Names may be repeated to produce\n" +
                  "repeated (multi-mapped) fields.",
                  "name", "value", DOTDOTDOT, DOTDOTDOT);
        }

        @Override
        Object makeIt(String[] names, Object[] values)
            throws FusionException
        {
            return mutableStruct(names, values, EMPTY_STRING_ARRAY);
        }
    }



    static final class StructMergeProc
        extends Procedure2
    {
        StructMergeProc()
        {
            //    "                                                                               |
            super("Returns a struct that has all the name-value elements of both arguments.  This\n" +
                  "will result in repeated fields if the names overlap or if one of the arguments\n" +
                  "has repeats. The result has the same type as the first argument.",
                  "struct1", "struct2");
        }

        @Override
        Object doApply(Evaluator eval, Object struct1, Object struct2)
            throws FusionException
        {
            checkStructArg(eval, 0, struct1, struct2);
            checkStructArg(eval, 1, struct1, struct2);

            return unsafeStructMerge(eval, struct1, struct2);
        }
    }

    static final class StructMergeMProc
        extends Procedure2
    {
        StructMergeMProc()
        {
            //    "                                                                               |
            super("Returns a struct that has all the name-value elements of both arguments,\n" +
                  "mutating the first argument when possible. This will result in repeated fields\n" +
                  "if the names overlap or if one of the arguments has repeats.  The result has\n" +
                  "the same type as the first argument.",
                  "struct1", "struct2");
        }

        @Override
        Object doApply(Evaluator eval, Object struct1, Object struct2)
            throws FusionException
        {
            checkStructArg(eval, 0, struct1, struct2);
            checkStructArg(eval, 1, struct1, struct2);

            return unsafeStructMergeM(eval, struct1, struct2);
        }
    }



    static final class StructMerge1Proc
        extends Procedure2
    {
        StructMerge1Proc()
        {
            //    "                                                                               |
            super("Functionally merges the mappings of `struct1` and `struct2`, removing repeated\n" +
                  "fields. Mappings from `struct2` will replace those from `struct1` with the\n" +
                  "same key. If there are repeated fields, one is selected arbitrarily.\n" +
                  "The result has the same type as the first argument.",
                  "struct1", "struct2");
        }

        @Override
        Object doApply(Evaluator eval, Object struct1, Object struct2)
            throws FusionException
        {
            checkStructArg(eval, 0, struct1, struct2);
            checkStructArg(eval, 1, struct1, struct2);

            return unsafeStructMerge1(eval, struct1, struct2);
        }
    }

    static final class StructMerge1MProc
        extends Procedure2
    {
        StructMerge1MProc()
        {
            //    "                                                                               |
            super("Merges the mappings of `struct1` and `struct2`, removing repeated fields and\n" +
                  "mutating the first argument when possible. Mappings from `struct2` will\n" +
                  "replace those from `struct1` with the same key.  If there are repeated fields,\n" +
                  "one is selected arbitrarily. The result has the same type as the first\n" +
                  "argument.",
                  "struct1", "struct2");
        }

        @Override
        Object doApply(Evaluator eval, Object struct1, Object struct2)
            throws FusionException
        {
            checkStructArg(eval, 0, struct1, struct2);
            checkStructArg(eval, 1, struct1, struct2);

            return unsafeStructMerge1M(eval, struct1, struct2);
        }
    }



    static abstract class AbstractZipProc
        extends Procedure
    {
        AbstractZipProc(String docBody)
        {
            //    "                                                                               |
            super(docBody,
                  "names", "values");
        }

        final Map<String,Object> _doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(2, args);
            Object names =  checkListArg(eval, 0, args);
            Object values = checkListArg(eval, 1, args);

            Iterator<?> fieldIterator = unsafeJavaIterate(eval, names);
            Iterator<?> valueIterator = unsafeJavaIterate(eval, values);

            HashMap<String, Object> map = new HashMap<String, Object>();

            while (fieldIterator.hasNext() && valueIterator.hasNext())
            {
                Object nameObj = fieldIterator.next();
                String name = copyTextToJavaString(nameObj);
                if (name == null || name.isEmpty())
                {
                    String expectation =
                        "sequence of non-empty strings or symbols";
                    throw new ArgTypeFailure(identify(), expectation, 0, args);
                }

                Object valueObj = valueIterator.next();
                structImplAdd(map, name, valueObj);
            }

            return map;
        }
    }


    static final class StructZipProc
        extends AbstractZipProc
    {
        StructZipProc()
        {
            //    "                                                                               |
            super("Constructs an immutable struct from a list of field names and a list of values.\n" +
                  "If the lists have unequal lengths, only the first _n_ elements will be used\n" +
                  "where _n_ is the shorter length.\n" +
                  "The names must be non-empty strings or symbols.\n" +
                  "\n" +
                  "    (struct_zip [\"f\", \"g\"] [1, 2])  => {f:1,g:2}\n" +
                  "    (struct_zip [\"f\", \"f\"] [1, 2])  => {f:1,f:2}\n" +
                  "    (struct_zip [\"f\"] [1, 2])       => {f:1}");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return immutableStruct(_doApply(eval, args));
        }
    }


    static final class MutableStructZipProc
        extends AbstractZipProc
    {
        MutableStructZipProc()
        {
            //    "                                                                               |
            super("Constructs a mutable struct from a list of field names and a list of values.\n" +
                  "If the lists have unequal lengths, only the first _n_ elements will be used\n" +
                  "where _n_ is the shorter length.\n" +
                  "The names must be non-empty strings or symbols..\n" +
                  "\n" +
                  "    (mutable_struct_zip [\"f\", \"g\"] [1, 2])  => {f:1,g:2}\n" +
                  "    (mutable_struct_zip [\"f\", \"f\"] [1, 2])  => {f:1,f:2}\n" +
                  "    (mutable_struct_zip [\"f\"] [1, 2])       => {f:1}");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return mutableStruct(_doApply(eval, args));
        }
    }



    abstract static class BaseKeysProc
        extends Procedure
    {
        BaseKeysProc(String doc, String... argNames)
        {
            super(doc, argNames);
        }

        abstract Object doIt(Evaluator eval, Object struct, String[] keys)
            throws FusionException;

        @Override
        final Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityAtLeast(1, args);

            Object struct = checkStructArg(eval, 0, args);

            String[] keys = new String[args.length - 1];
            for (int i = 1; i < args.length; i++)
            {
                keys[i-1] = checkTextArg(i, args);
            }

            return doIt(eval, struct, keys);
        }
    }



    static final class RemoveKeysProc
        extends BaseKeysProc
    {
        RemoveKeysProc()
        {
            //    "                                                                               |
            super("Returns a struct derived from `struct` without fields with the given `name`s.\n" +
                  "If the input is `null.struct` then the result is `null.struct`.  The result\n" +
                  "will have the same annotations as `struct`.",
                  "struct", "name", DOTDOTDOT);
        }

        @Override
        Object doIt(Evaluator eval, Object struct, String[] keys)
            throws FusionException
        {
            return unsafeStructRemoveKeys(eval, struct, keys);
        }
    }



    static final class RemoveKeysMProc
        extends BaseKeysProc
    {
        RemoveKeysMProc()
        {
            //    "                                                                               |
            super("Returns a struct similar to `struct` without fields with the given `name`s.\n" +
                  "The result may share structure with the input, which may be mutated.\n" +
                  "If the input is `null.struct` then the result is `null.struct`.  The result\n" +
                  "will have the same annotations as `struct`.",
                  "struct", "name", DOTDOTDOT);
        }

        @Override
        Object doIt(Evaluator eval, Object struct, String[] keys)
            throws FusionException
        {
             return unsafeStructRemoveKeysM(eval, struct, keys);
        }
    }



    static final class RetainKeysProc
        extends BaseKeysProc
    {
        RetainKeysProc()
        {
            //    "                                                                               |
            super("Returns a struct derived from `struct` with _only_ fields with the given\n" +
                  "`name`s.  If the input is `null.struct` then the result is `null.struct`.  The\n" +
                  "result will have the same annotations as `struct`.",
                  "struct", "name", DOTDOTDOT);
        }

        @Override
        Object doIt(Evaluator eval, Object struct, String[] keys)
            throws FusionException
        {
            return unsafeStructRetainKeys(eval, struct, keys);
        }
    }



    static final class RetainKeysMProc
        extends BaseKeysProc
    {
        RetainKeysMProc()
        {
            //    "                                                                               |
            super("Returns a struct similar to `struct` with _only_ fields with the given\n" +
                  "`name`s. The result may share structure with the input, which may be mutated.\n" +
                  "If the input is `null.struct` then the result is `null.struct`. The result\n" +
                  "will have the same annotations as `struct`.",
                  "struct", "name", DOTDOTDOT);
        }

        @Override
        Object doIt(Evaluator eval, Object struct, String[] keys)
            throws FusionException
        {
            return unsafeStructRetainKeysM(eval, struct, keys);
        }
    }
}
