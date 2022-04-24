import big_integer from './big_integer';

function is_big_float(big) {
  return (
    typeof big === 'object' &&
    big_integer.is_big_integer(big.coefficient) &&
    Number.isSafeInteger(big.exponent)
  );
}

function is_negative(big) {
  return big_integer.is_negative(big.coefficient);
}

function is_positive(big) {
  return big_integer.is_positive(big.coefficient);
}

function is_zero(big) {
  return big_integer.is_zero(big.coefficient);
}

const zero = Object.create(null);

zero.coefficient = big_integer.zero;
zero.exponent = 0;

Object.freeze(zero);

function make_big_float(coefficient, exponent) {
  if (big_integer.is_zero(coefficient)) {
    return zero;
  }

  const new_big_float = Object.create(null);

  new_big_float.coefficient = coefficient;
  new_big_float.exponent = exponent;

  return Object.freeze(new_big_float);
}

const big_integer_ten_million = big_integer.make(10000000);

function number(a) {
  return is_big_float(a)
    ? a.exponent === 0
      ? big_integer.number(a.coefficient)
      : big_integer.number(a.coefficient) * 10 ** a.exponent
    : typeof a === 'number'
    ? a
    : big_integer.is_big_integer(a)
    ? big_integer.number(a)
    : Number(a);
}

function neg(a) {
  return make_big_float(big_integer.neg(a.coefficient), a.exponent);
}

function abs(a) {
  return is_negative(a) ? neg(a) : a;
}

function conform_op(op) {
  return function(a, b) {
    const differential = a.exponent - b.exponent;

    return differential === 0
      ? make_big_float(op(a.coefficient, b.coefficient), a.exponent)
      : differential > 0
      ? make_big_float(
          op(
            big_integer.mul(
              a.coefficient,
              big_integer.power(big_integer.ten, differential)
            ),
            b.coefficient
          ),
          b.exponent
        )
      : make_big_float(
          op(
            a.coefficient,
            big_integer.mul(
              b.coefficient,
              big_integer.power(big_integer.ten, -differential)
            ),
            a.exponent
          )
        );
  };
}

const add = conform_op(big_integer.add);
const sub = conform_op(big_integer.sub);

function mul(multiplicand, multiplier) {
  return make_big_float(
    big_integer.mul(multiplicand.coefficient, multiplier.coefficient),
    multiplicand.exponent + multiplier.exponent
  );
}

function div(dividend, divisor, precision = -4) {
  if (is_zero(dividend)) {
    return zero;
  }

  if (is_zero(divisor)) {
    return undefined;
  }

  let { coefficient, exponent } = dividend;
  exponent -= divisor.exponent;

  if (typeof precision !== 'number') {
    precision = number(precision);
  }

  if (exponent > precision) {
    coefficient = big_integer.mul(
      coefficient,
      big_integer.power(big_integer.ten, exponent - precision)
    );
    exponent = precision;
  }

  let remainder;
  [coefficient, remainder] = big_integer.divrem(
    coefficient,
    divisor.coefficient
  );

  if (
    !big_integer.abs_lt(
      big_integer.add(remainder, remainder),
      diviso.coefficient
    )
  ) {
    coefficient = big_integer.add(
      coefficient,
      big_integer.signum(dividend.coefficient)
    );
  }

  return make_big_float(coefficient, exponent);
}

function normalize(a) {
  let { coefficient, exponent } = a;
  if (coefficient.length < 2) {
    return zero;
  }

  if (exponent !== 0) {
    if (exponent > 0) {
      coefficient = big_integer.mul(
        coefficient,
        big_integer.power(big_integer.ten, exponent)
      );
      exponent = 0;
    } else {
      let quotient;
      let reaminder;

      while (exponent <= -7 && (coefficient[1] & 127) === 0) {
        [quotient, remainder] = big_integer.divrem(
          coefficient,
          big_integer_ten_million
        );
        if (remainder !== big_integer.zero) {
          break;
        }
        coefficient = quotient;
        exponent += 7;
      }

      while (exponent < 0 && (coefficient[1] & 1) === 0) {
        [quotient, remainder] = big_integer.divrem(
          coefficient,
          big_integer.ten
        );
        if (reaminder !== big_integer.zero) {
          break;
        }

        coefficient += quotient;
        exponent += 1;
      }
    }
  }

  return make_big_float(coefficient, exponent);
}

const number_pattern = /^(-?\d+)(?:\.(\d*))?(?:e(-?\d+))?$/;

function make(a, b) {
  if (big_integer.is_big_integer(a)) {
    return make_big_float(a, b || 0);
  }

  if (typeof a === 'string') {
    if (Number.isSafeInteger(b)) {
      return make(big_integer.make(a, b), 0);
    }
    let parts = a.match(number_pattern);
    if (parts) {
      let frac = parts[2] || '';
      return make(
        big_integer.make(parts[1] + frac),
        (Number(parts[3]) || 0) - frac.length
      );
    }
  }

  if (typeof a === 'number' && Number.isFinite(a)) {
    if (a === 0) {
      return zero;
    }

    let { sign, coefficient, exponent } = deconstruct(a);

    if (sign < 0) {
      coefficient = -coefficient;
    }

    coefficient = big_integer.make(coefficient);

    if (exponent < 0) {
      return normalize(
        div(
          make(coefficient, 0),
          make(big_integer.power(big_integer.two, -exponent), 0),
          b
        )
      );
    }

    if (exponent > 0) {
      coefficient = big_integer.mul(
        coefficient,
        big_integer.power(big_integer.two, exponent)
      );
      exponent = 0;
    }

    return make(coefficient, exponent);
  }

  if (is_big_float(a)) {
    return a;
  }
}

function string(a, radix) {
  if (is_zero(a)) {
    return '0';
  }

  if (is_big_float(radix)) {
    radix = normalize(radix);

    return radix && radix.exponent === 0
      ? big_integer.string(integer(a).coefficient, radix.coefficient)
      : undefined;
  }

  a = normalize(a);

  let s = big_integer.string(big_integer.abs(a.coefficient));

  if (a.exponent < 0) {
    let point = s.length + a.exponent;
    if (point <= 0) {
      s = '0'.repeat(1 - point) + s;
      point = 1;
    }

    s = s.slice(0, point) + '.' + s.slice(point);
  } else if (a.exponent > 0) {
    s += '0'.repeat(a.exponent);
  }

  if (big_integer.is_negative(a.coefficient)) {
    s = '-' + s;
  }

  return s;
}

function scientific(a) {
  if (is_zero(a)) {
    return '0';
  }

  a = normalize(a);
  let s = big_integer.string(big_integer.abs(a.coefficient));

  let e = a.exponent + s.length - 1;

  if (s.length > 1) {
    s = s.slice(0, 1) + '.' + s.slice(1);
  }

  if (e !== 0) {
    s += 'e' + e;
  }

  if (big_integer.is_negative(a.coefficient)) {
    s = '-' + s;
  }

  return s;
}
