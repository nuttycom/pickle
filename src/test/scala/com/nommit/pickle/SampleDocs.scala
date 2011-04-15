package com.nommit.pickle

object SampleDocs {
  val pancakes = """
    @*[@units[cup # @type[volume]] # @id[cup]]
    @*[@units[tsp # @type[volume]] # @id[tsp]]

    @[ingredients]
      @*[@volume[1 # @^[cup]]     Whole wheat flour # @id[flour]]
      @*[@volume[1 1/2 # @^[tsp]] Baking Powder     # @id[baking powder]]
      @*[@volume[1/2 # @^[tsp]]   Salt              # @id[salt]]
      @*[@quantity[2]             Eggs              # @id[egg]]
      @*[@volume[1 # @^[cup]]     Milk              # @id[milk]]
      @*[@volume[1/4 # @^[cup]]   Oil               # @id[oil]]
    @/

    @[steps]
      Mix together @^[ingredients > flour], @^[ingredients > baking powder] and @^[ingredients > salt] in a bowl. @@
      Separate the @^[ingredients > egg], reserving the @part[whites # @^[ingredients > egg]].      @@
      Combine @part[yolks # @^[ingredients > egg]] with the @^[ingredients > milk] and @^[ingredients > oil].
    @/
  """

  val employees = """ +
    @employees[
      @name[John Doe]
      @address[221 B Baker Street]
      |
      @name[Mary Jane]
      @address[221 C Baker Street]
      #
      @collectionType[Set] @elementType[Employee]
    ]
  """
}