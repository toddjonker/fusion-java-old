// Copyright (c) 2018-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class FunctionalHashTrieTest
{
    private Map<Object, Object> baselineMap;
    private FunctionalHashTrie<Object, Object> fht;

    public void setup(int size)
    {
        baselineMap = new HashMap<>();
        for (int i = 0; i < size; i++)
        {
            baselineMap.put(new Object(), new Object());
        }
        fht = FunctionalHashTrie.create(baselineMap);
    }

    private void doRemoval()
    {
        Object[] keys = baselineMap.keySet().toArray();
        Set<Object> keysToRemove = new LinkedHashSet<>(keys.length / 2);
        for (int i = 0; i < keys.length / 2; i++)
        {
            keysToRemove.add(keys[i]);
        }

        baselineMap.keySet().removeAll(keysToRemove);

        for (Object key : keysToRemove)
        {
            fht = fht.without(key);
        }
    }

    private void doAddition()
    {
        int toAdd = baselineMap.size();
        for (int i = 0; i < toAdd; i++)
        {
            Map.Entry entry = new AbstractMap.SimpleEntry<>(new Object(), new Object());

            baselineMap.put(entry.getKey(), entry.getValue());
            fht = fht.with(entry.getKey(), entry.getValue());
        }
    }

    private void checkSizing()
    {
        assertEquals(baselineMap.size(), fht.size());
    }

    private void checkIterators()
    {
        Set<Object> keySet = fht.keySet();
        Set<Object> baselineSet = baselineMap.keySet();
        assertEquals(baselineSet, keySet);

        Set<Object> values = new HashSet<>(fht.values());
        Set<Object> baselineValues = new HashSet<>(baselineMap.values());
        assertEquals(baselineValues, values);

        Set<Map.Entry<Object, Object>> entrySet = fht.entrySet();
        Set<Map.Entry<Object, Object>> baselineEntrySet = baselineMap.entrySet();
        assertEquals(baselineEntrySet, entrySet);
    }

    private void compareEntries()
    {
        for (Map.Entry entry : baselineMap.entrySet())
        {
            Object baselineValue = entry.getValue();
            assertEquals(baselineValue, fht.get(entry.getKey()));
        }
    }

    private void compareWithBaseline()
    {
        checkSizing();
        checkIterators();
        compareEntries();
        assertEquals(baselineMap, fht);
        assertEquals(baselineMap.hashCode(), fht.hashCode());
    }

    private void performTests()
    {
        compareWithBaseline();
        doRemoval();
        compareWithBaseline();
        doAddition();
        compareWithBaseline();
    }

    @Test
    public void checkEmpty()
    {
        setup(0);
        assertTrue(fht.isEmpty());
        assertEquals(baselineMap.toString(), fht.toString());
        performTests();

        FunctionalHashTrie without = fht.without("anything");
        assertTrue(fht == without);
        assertTrue(fht == FunctionalHashTrie.empty());
    }

    @Test
    public void checkSingle()
    {
        setup(1);
        assertEquals(baselineMap.toString(), fht.toString());
        performTests();
    }

    @Test
    public void checkSmall()
    {
        setup(10);
        performTests();
    }

    @Test
    public void checkVeryLarge()
    {
        setup(10000);
        performTests();
    }

    @Test
    public void checkVeryVeryLarge()
    {
        setup(1000000);
        performTests();
    }


    @Test
    public void checkNullKeys()
    {
        setup(1);

        final String failureMessage = "Should have raised a NullPointerException";
        try
        {
            fht.with(null, "foo");
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with("foo", null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with(null, null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.without(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.get(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.containsKey(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.containsValue(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }
    }


    @Test
    public void compareMutableCreateVsSequential()
    {
        setup(10000);

        FunctionalHashTrie seq = FunctionalHashTrie.empty();
        for (Map.Entry e : fht.entrySet())
        {
            seq = seq.with(e.getKey(), e.getValue());
        }

        assertEquals(fht.size(), seq.size());
        for (Map.Entry e : fht.entrySet())
        {
            Object key = e.getKey();
            assertEquals(fht.get(key), seq.get(key));
        }
    }
}
