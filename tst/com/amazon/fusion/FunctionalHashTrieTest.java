// Copyright (c) 2018-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FunctionalHashTrie.fromSelectedKeys;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.AbstractMap.SimpleEntry;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.junit.Test;

public class FunctionalHashTrieTest
{
    private HashMap<Object, Object> baselineMap;
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
            Map.Entry entry = new SimpleEntry<>(new Object(), new Object());

            baselineMap.put(entry.getKey(), entry.getValue());
            fht = fht.with(entry.getKey(), entry.getValue());
        }
    }

    private void checkSizing()
    {
        assertEquals(baselineMap.size(), fht.size());
    }

    private void checkKeys()
    {
        Set<Object> keySet = fht.keySet();
        Set<Object> baselineSet = baselineMap.keySet();
        assertEquals(baselineSet, keySet);
    }

    private void compareEntries()
    {
        Map<Object, Object> remainder = (Map<Object, Object>) baselineMap.clone();

        for (Map.Entry<Object, Object> entry : fht)
        {
            assertEquals(entry.getValue(), remainder.remove(entry.getKey()));
        }

        assertEquals("elements remaining", 0, remainder.size());
    }

    private void compareWithBaseline()
    {
        checkSizing();
        checkKeys();
        compareEntries();
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
        assertSame(FunctionalHashTrie.empty(), fht);
        performTests();

        FunctionalHashTrie without = fht.without("anything");
        assertSame(without, fht);
        assertSame(FunctionalHashTrie.empty(), fht);
    }

    @Test
    public void checkSingle()
    {
        setup(1);
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
    }


    @Test
    public void compareMutableCreateVsSequential()
    {
        setup(10000);

        FunctionalHashTrie seq = FunctionalHashTrie.empty();
        for (Map.Entry e : fht)
        {
            seq = seq.with(e.getKey(), e.getValue());
        }

        assertEquals(fht.size(), seq.size());
        for (Map.Entry e : fht)
        {
            Object key = e.getKey();
            assertEquals(fht.get(key), seq.get(key));
        }
    }

    /** Key-merge function that simply uses the new value. */
    public static final BiFunction REPLACE = new BiFunction() {
        @Override
        public Object apply(Object o, Object o2) {
            return o2;
        }
    };

    @Test
    public void testMergeWithReplacement()
    {
        Map.Entry[] entries = { new SimpleEntry<>("f", "old"),
                                new SimpleEntry<>("f", "new") };
        FunctionalHashTrie trie = FunctionalHashTrie.merge(entries, REPLACE);
        assertEquals(1, trie.size());
        assertEquals("new", trie.get("f"));
    }

    @Test
    public void testNoopInsertion()
    {
        FunctionalHashTrie trie1 = FunctionalHashTrie.empty().with(1, 1);
        FunctionalHashTrie trie2 = trie1.with(1, 1);
        assertSame(trie1, trie2);
    }


    @Test
    public void fromSelectedKeysReturnsSubset()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with(1, 1).with(2, 2).with(3, 3);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{ 1, 3, 5 });

        assertEquals(2,    t.size());
        assertEquals(1,    t.get(1));
        assertEquals(null, t.get(2));
        assertEquals(3,    t.get(3));
    }

    @Test
    public void fromSelectedKeysReturnsEmptySingleton()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with(1, 1).with(2, 2);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{});
        assertSame(FunctionalHashTrie.empty(), t);

        t = fromSelectedKeys(origin, new Object[]{ 3, 4 });
        assertSame(FunctionalHashTrie.empty(), t);
    }


    @Test
    public void withoutKeysGivenKeysReturnsSubset()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with(1, 1).with(2, 2).with(3, 3);

        FunctionalHashTrie t = origin.withoutKeys(new Object[]{ 1, 3, 5 });

        assertEquals(1,    t.size());
        assertEquals(null, t.get(1));
        assertEquals(2,    t.get(2));
        assertEquals(null, t.get(3));
    }

    @Test
    public void withoutKeysGivenAllKeysReturnsSingleton()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with(1, 1).with(2, 2);
        FunctionalHashTrie t = origin.withoutKeys(new Object[]{1, 2});
        assertSame(FunctionalHashTrie.empty(), t);
    }
}
