const LRU = require('lru-cache');

// exports.uncurry2_ = function(f)
// {
//     return function(arg1)
//     {
//         return function(arg2)
//             {
//                 console.log("Applying f to " + arg1.toString() + " " + arg2.toString() + ", returns " + (f(arg1, arg2)).toString() );
//                 return f(arg1, arg2);
//             }
//     }
// }

exports.uncurry2_ = function(f)
{
    return function(a, b)
        {
            return f(a)(b);
        }
}

exports.newCache_ = 
    function (options)
    {
        return new LRU( options );
    }

//set(key, value, [{ size, sizeCalculation, ttl, noDisposeOnSet }])
exports.set_ = 
    function( key, value, cache, options)
    {
        return cache.set(key, value, options);
    }

//set(key, value, [{ size, sizeCalculation, ttl, noDisposeOnSet }])
exports.set__ = 
function( key, value, cache)
{
    return cache.set(key, value);
}

// get(key, { updateAgeOnGet, allowStale } = {}) => value
exports.get_ = 
function( key, options, nothing, just, cache )
{
    const r = cache.get(key, options);
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

exports.size_ = 
function( cache )
{
    return cache.size;
}

exports.calculatedSize_ = 
function( cache )
{
    return cache.calculatedSize;
}

// peek(key, { allowStale } = {}) => value
exports.peek_ = 
function( key, options, nothing, just, cache )
{
    const r = cache.peek(key, options);
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

// has(key, { updateAgeOnHas } = {}) => Boolean
exports.has_ = 
function(key, options, cache)
{
    const r = cache.has(key, options);
    if (r)
    {
        return true;
    }
    else
    {
        return false;
    }
}

exports.delete_ = 
function(key, cache)
{
    return cache.delete(key);
}

exports.clear_ = 
function(unit, cache)
{
    cache.clear();
    return unit;
}

exports.keys_ =
function(cache)
{
    return cache.keys();
}

exports.rkeys_ =
function(cache)
{
    return cache.rkeys();
}

exports.values_ =
function(cache)
{
    return cache.values();
}
exports.rvalues_ =
function(cache)
{
    return cache.rvalues();
}

exports.entries_ = 
function( tuple, cache )
{
    const entryIterator = cache.entries();
    return {
        next: function()
            {
                const n = entryIterator.next();
                if (n.done)
                {
                    return n;
                }
                else
                {
                    n.value = tuple( n.value[0]) (n.value[1]);
                    return n;
                }
            }
    }
}

exports.rentries_ = 
function( tuple, cache )
{
    const entryIterator = cache.rentries();
    return {
        next: function()
            {
                const n = entryIterator.next();
                if (n.done)
                {
                    return n;
                }
                else
                {
                    n.value = tuple( n.value[0]) (n.value[1]);
                    return n;
                }
            }
    }
}