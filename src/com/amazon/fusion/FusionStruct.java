// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.unsafeJavaIterate;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.FusionWrite.dispatchIonize;
import static com.amazon.fusion.FusionWrite.dispatchWrite;
import static com.amazon.fusion.FusionWrite.safeWriteToString;
import static com.amazon.ion.util.IonTextUtils.printSymbol;
import static java.lang.Boolean.TRUE;
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

        /**
         * Visits each field in the struct, stopping as soon as the visitation
         * returns non-null.
         */
        abstract void visitFields(StructFieldVisitor visitor)
            throws FusionException;

        // Return type isn't really right
        abstract ImmutableStruct transformFields(StructFieldVisitor visitor)
            throws FusionException;

        abstract boolean hasKey(Evaluator eval, String key)
            throws FusionException;

        /** Returns void if the field doesn't exist. */
        abstract Object dot(Evaluator eval, String field)
            throws FusionException;

        /**
         * Finds the value for a given name.
         *
         * @param def the default result. If its a procedure, it's called with
         * no arguments to determine the result. Otherwise it's returned as-is.
         */
        abstract Object ref(Evaluator eval, String name, Object def)
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
        boolean hasKey(Evaluator eval, String key)
            throws FusionException
        {
            return false;
        }

        @Override
        Object dot(Evaluator eval, String field)
            throws FusionException
        {
            return voidValue(eval);
        }

        @Override
        Object ref(Evaluator eval, String name, Object def)
            throws FusionException
        {
            return bounceDefaultResult(eval, def);
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

        @Override
        boolean hasKey(Evaluator eval, String key)
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
        Object ref(Evaluator eval, String name, Object def)
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
            String key = checkTextArg(1, args);    // TODO reduce safety checks
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
            String name = checkTextArg(1, args);   // TODO reduce safety checks
            return s.ref(eval, name, args[2]);
        }
    }



    static final class UnsafeStructVisitProc
        extends Procedure2
    {
        UnsafeStructVisitProc()
        {
            //    "                                                                               |
            super("*UNSUPPORTED!*  Applies `proc` to the fields of the `struct`, stopping when\n" +
                  "the result of the call is truthy.",
                  "proc", "struct");
        }

        @Override
        Object doApply(final Evaluator eval, Object proc, Object struct)
            throws FusionException
        {
            final Procedure p = (Procedure) proc;

            StructFieldVisitor visitor = new StructFieldVisitor()
            {
                @Override
                public Object visit(String name, Object value)
                    throws FusionException
                {
                    Object nameSym = eval.newSymbol(name);
                    Object result = eval.callNonTail(p, nameSym, value);
                    return (isTruthy(eval, result) ? TRUE : null);
                }
            };

            unsafeStructFieldVisit(eval, struct, visitor);

            return struct;
        }
    }



    static final class StructProc
        extends Procedure
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

        @Override
        Object doApply(Evaluator eval, Object[] args)
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

            return immutableStruct(names, values, EMPTY_STRING_ARRAY);
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
                  "has repeats.",
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



    static final class StructZipProc
        extends Procedure2
    {
        StructZipProc()
        {
            //    "                                                                               |
            super("Constructs a struct from a list of field names and a list of values.  The names\n" +
                  "must be non-empty strings or symbols.",
                  "names", "values");
        }


        @Override
        Object doApply(Evaluator eval, Object names, Object values)
            throws FusionException
        {
            checkListArg(eval, 0, names, values);
            checkListArg(eval, 1, names, values);

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
                        "non-empty string or symbol in field-name sequence";
                    throw new ResultFailure(identify(), expectation,
                                            safeWriteToString(eval, nameObj));
                }

                Object valueObj = valueIterator.next();
                structImplAdd(map, name, valueObj);
            }

            return immutableStruct(map);
        }
    }



    static final class RemoveKeysProc
        extends Procedure
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
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityAtLeast(1, args);

            Object struct = checkStructArg(eval, 0, args);

            if (args.length == 1) return struct;

            String[] keys = new String[args.length - 1];
            for (int i = 1; i < args.length; i++)
            {
                keys[i-1] = checkTextArg(i, args);
            }

            return unsafeStructRemoveKey(eval, struct, keys);
        }
    }


    static final class RetainKeysProc
        extends Procedure
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
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityAtLeast(1, args);

            Object struct = checkStructArg(eval, 0, args);

            String[] keys = new String[args.length - 1];
            for (int i = 1; i < args.length; i++)
            {
                keys[i-1] = checkTextArg(i, args);
            }

            return unsafeStructRetainKey(eval, struct, keys);
        }
    }

}
