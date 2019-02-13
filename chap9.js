class LazySeq {
  constructor(low, high) {
    this.low = low;
    this.high = high;
  }
  
  head() {
    return this.low;
  }
  
  tail() {
    return new LazySeq(this.low + 1, this.high);
  }
  
  isEmpty() {
    return (this.low > this.high);
  }
}

const seqToList = (seq, acc=[]) => {
  if (seq.isEmpty()) {
    return acc;
  } else {
    return seqToList(seq.tail(), acc.concat([seq.head()]));
  }
}

const take = (seq, len, acc=[]) => {
  if (seq.isEmpty() || len <= 0) {
    return acc;
  } else {
    return take(seq.tail(), len - 1, acc.concat([seq.head()]));
  }
}

console.log(seqToList(new LazySeq(5, 9)));
var aa = new LazySeq(5, 9);
console.log(take(aa, 2));
