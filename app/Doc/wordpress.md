WordPress uses a variety of query parameters to filter and display different types of content. These query parameters are often hidden behind pretty permalinks, but they are still used internally by WordPress to determine what content to load.

Here are some of the main query parameters WordPress uses:

    p: The post ID to display. For example, ?p=123 would display the post with the ID of 123.
    page_id: The ID of a page to display.
    cat: The category ID to display. For example, ?cat=4 would display a list of posts in category 4.
    tag: The slug of a tag to display. For example, ?tag=flowers would display a list of posts with the tag "flowers".
    s: The search term. For example, ?s=wordpress would display a list of posts that include the word "wordpress".
    m: The date archive. For example, ?m=202301 would display posts from January 2023.
    author: The author ID. For example, ?author=1 would display a list of posts by the author with the ID of 1.
    name: The post slug (used for pretty permalinks). For example, ?name=my-post would display the post with the slug "my-post".
    WordPress uses these query parameters (and others) in WP_Query, which is the main class for querying the database for content. When WordPress receives a request, it parses the request URL and translates it into query parameters, which are then used to instantiate a WP_Query object and fetch the requested content.

Keep in mind, these parameters might be not visible in the URL if pretty permalinks are enabled. Instead, they are derived from the permalink structure. For example, the URL /2023/01/my-post would be translated into ?m=202301&name=my-post.






The following steps outline a high-level overview of what happens in WordPress's index.php file and the sequence of functions leading to the creation of the WP_Query object.

The index.php file begins by defining the WP_USE_THEMES constant as true. This constant tells WordPress to load the theme's template files.

It then includes the wp-blog-header.php file. This file is responsible for setting up the WordPress environment and loading the template.

```php
define('WP_USE_THEMES', true);
require( dirname( __FILE__ ) . '/wp-blog-header.php' );
```

Inside wp-blog-header.php, WordPress includes wp-load.php to load the WordPress environment and configuration.

wp-load.php then includes wp-config.php (which loads wp-settings.php) to get the WordPress settings.

wp-settings.php does a lot of the heavy lifting. It sets up default constants, includes the files for the rest of the WordPress core software, sets up default filters and actions, includes the active plugins, and initializes the $wp_locale and $wp objects.

wp-settings.php then calls wp() which is defined in wp-includes/functions.php. This function creates a new instance of the WP class (defined in wp-includes/class-wp.php) and calls its main() method.

php
```
function wp( $query_vars = '' ) {
    global $wp, $wp_query, $wp_the_query;
    $wp->main( $query_vars );
    // ...
}
```

The main() method of the WP class is where the magic happens. It sets up the WordPress Query ($wp_the_query), which is an instance of WP_Query. This involves parsing the request URL, turning it into query variables, and creating the WP_Query object.

php
```
function main( $query_args = '' ) {
    $this->init();
    $this->parse_request( $query_args );
    $this->send_headers();
    $this->query_posts();
    $this->handle_404();
    $this->register_globals();
    do_action( 'wp' );
}
```

Once the WP_Query object is created and the query variables are set, the query_posts() method is invoked. This method calls the query() method on the WP_Query object, passing it the query variables. The query() method then fetches the posts from the database that match those query variables.

After wp(), wp-blog-header.php decides which template file from the current theme should be used to display the page, using the template-loader.php file. This depends on the results of the WP_Query object and the WordPress Template Hierarchy.

Finally, the chosen template file is loaded, which generally includes a call to the_post() which sets up the post data and then the_content() which displays the content of the post.

This is a simplified overview of the process, and there are a lot more details to it, but this should give you an idea of how a request gets turned into a WP_Query object and ultimately a fully rendered page