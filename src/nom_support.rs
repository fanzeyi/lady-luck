pub type IResult<'a, O> = nom::IResult<&'a str, O, nom_supreme::error::ErrorTree<&'a str>>;
