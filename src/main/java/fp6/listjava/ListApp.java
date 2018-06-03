package fp6.listjava;

import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Java 中没有 sealed 关键字，无法保证 interface 的子类是限定的
 * Java 也没有模式匹配，因此判断实例类型时，应精确匹配 Nil 和 Cons.
 * 这里假定只有 Nil 和 Cons.
 * <p>
 * List 中各个方法都长得差不多，这是因为全部使用了模式匹配 + 递归实现。
 * <p>
 * 模式匹配 + 递归可以说是 FP 的精髓。
 *
 * @param <A>
 */
interface List<A> {
    /**
     * 可以使用 List.apply(1, 2, 3) 便捷地创建 List
     * 避免了 new Cons(1, new Cons(2, new Cons(3, Nil))
     *
     * @param args
     * @param <T>
     * @return
     */
    static <T> List<T> apply(T... args) {
        if (args.length == 0) return Nil.getInstance();
        else {
            T[] newArr = (T[]) new Object[args.length - 1];
            for (int i = 0; i < newArr.length; i++) {
                newArr[i] = args[i + 1];
            }
            return new Cons(args[0], apply(newArr));
        }
    }

    static List empty() {
        return Nil.getInstance();
    }

    default int length() {
        if (this instanceof Nil) return 0;
        else {
            Cons<A> cons = (Cons<A>) this;
            return 1 + cons.tail.length();
        }
    }

    default boolean isEmpty() {
        return this instanceof Nil;
    }


    default A sum(A initValue, BiFunction<A, A, A> f) {
        if (this instanceof Nil) {
            return initValue;
        } else { // instance of Cons
            Cons<A> cons = (Cons<A>) this;
            return f.apply(cons.header, cons.tail.sum(initValue, f));
        }
    }

    default void forEach(Consumer<A> cons) {
        if (this instanceof Cons) {
            Cons<A> c = (Cons<A>) this;
            cons.accept(c.header);
            c.tail.forEach(cons);
        }
    }


    default A head() {
        if (this instanceof Nil)
            throw new RuntimeException("Empty List!");
        else { // instance of Cons
            Cons<A> cons = (Cons<A>) this;
            return cons.header;
        }
    }

    default List<A> tail() {
        if (this instanceof Nil)
            throw new RuntimeException("Empty List!");
        else { // instance of Cons
            Cons<A> cons = (Cons<A>) this;
            return cons.tail;
        }
    }

    default List<A> take(int n) {
        if (n < 0) {
            throw new RuntimeException("index < 0!");
        } else if (n == 0) {
            return Nil.getInstance();
        } else {
            if (this instanceof Nil) {
                return Nil.getInstance();
            } else {
                Cons<A> cons = (Cons<A>) this;
                return new Cons(cons.header, cons.tail.take(n - 1));
            }
        }
    }

    /**
     * 注意 takeWhile 和 filter 的区别
     * takeWhile 从第一个开始取，遇到不满足条件的元素即停止处理并返回
     * filter 则全部遍历
     *
     * @param f
     * @return
     */
    default List<A> takeWhile(Predicate<A> f) {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return f.test(header)
                    ? new Cons(cons.header, tail.takeWhile(f))
                    : Nil.getInstance();
        }
    }

    default List<A> drop(int n) {
        if (n < 0) {
            throw new RuntimeException("index < 0!");
        } else if (n == 0) {
            return this;
        } else {
            if (this instanceof Nil) {
                return Nil.getInstance();
            } else {
                Cons<A> cons = (Cons<A>) this;
                return cons.tail.drop(n - 1);
            }
        }
    }

    default List<A> dropWhile(Predicate<A> f) {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return f.test(header) ? tail.dropWhile(f) : this;
        }
    }

    default List<A> addAll(List<A> b) {
        if (this instanceof Nil) {
            return b;
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return new Cons(header, tail.addAll(b));
        }
    }

    /**
     * init 是去除了尾部一个元素的剩余部分
     *
     * @return
     */
    default List<A> init() {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return new Cons(header, tail.init());
        }
    }

    default <B> List<B> map(Function<A, B> func) {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return new Cons(func.apply(header), tail.map(func));
        }
    }

    default <B> List<B> flatMap(Function<A, List<B>> func) {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return func.apply(header).addAll(tail.flatMap(func));
        }
    }

    default List<A> filter(Predicate<A> f) {
        if (this instanceof Nil) {
            return Nil.getInstance();
        } else {
            Cons<A> cons = (Cons<A>) this;
            A header = cons.header;
            List<A> tail = cons.tail;
            return f.test(header) ? new Cons(header, tail.filter(f)) : tail.filter(f);
        }
    }
}


class Nil implements List {
    private Nil() {
    }

    static Nil getInstance() {
        return Holder.instance;
    }

    private static class Holder {
        private static Nil instance;

        static {
            instance = new Nil();
        }
    }
}

class Cons<A> implements List<A> {
    final A header;
    final List<A> tail;

    Cons(A h, List<A> t) {
        this.header = h;
        this.tail = t;
    }

}

public class ListApp {
    public static void main(String[] args) {
        List<Integer> intList = List.apply(1, 2, 3);
        System.out.println("length" + intList.length());
        System.out.println(intList.sum(0, (a, b) -> a + b));

        List<String> strList = List.apply("a", "b", "cc");
        String r2 = strList.sum("", String::concat);
        System.out.println(r2);

        List empty = List.empty();
        System.out.println("empty: " + empty.isEmpty());

        List<String> data = List.apply("hello", "how", "are", "you");
        System.out.println("head " + data.head());
        data.tail().forEach(System.out::println);

        System.out.println("--------------------");
        data.filter(x -> x.startsWith("h")).forEach(System.out::println);

        data.map(String::toUpperCase).forEach(System.out::println);

        data.flatMap(x -> {
            char[] chars = x.toCharArray();
            Character[] cs = new Character[chars.length];
            for (int i = 0; i < cs.length; i++) {
                cs[i] = Character.valueOf(chars[i]);
            }
            return List.apply(cs);
        }).forEach(x -> System.out.print(x.toString() + "-"));
    }
}