type t = {
    id: int,
    friendlyName: string,
};

type scope = {
    mutable lastId: int
}

let createScope = () => {
    let ret: scope = {
        lastId: 0,
    };
    ret;
}

let newId = (~friendlyName:option(string)=?, scope: scope) => {
    let id = scope.lastId + 1;
    scope.lastId = id;

    let friendlyName = switch(friendlyName) {
    | Some(x) => x   
    | None => "component" ++ string_of_int(id);
    };

    let ret: t = {
        id,
        friendlyName,
    };
    ret;
};
