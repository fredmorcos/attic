roots a b c =
      let sdisc = sqrt (b * b - 4 * a * c)
          div   = 2 * a
      in  ((-b + sdisc) / div,
           (-b - sdisc) / div)
