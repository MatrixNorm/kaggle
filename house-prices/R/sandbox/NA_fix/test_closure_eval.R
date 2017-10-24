
kekMakerFactory = function (patch) {
  
  function (condition) {
    browser()
    function (df.data) {
      eval(condition, df.data)
    }
  }
}


kekMaker = kekMakerFactory(function (data) {
  11111
})

abc = c(4, 5, 6)
kekSadExample = kekMaker(abc == 'bogpill')
#kekHappyExample = kekMaker(substitute(abc == 'bogpill'))

#print(kekHappyExample(data.frame(abc=c(1, 2, 3))))
print(kekSadExample(data.frame(abc=c(1, 2, 3))))