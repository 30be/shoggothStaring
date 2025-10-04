fetch('/search.json')
  .then(response => response.json())
  .then(posts => {
    const input = document.getElementById('search-input');
    const resultsContainer = document.getElementById('search-results')
    const posts = posts.map(post => ({
      ...post,
      postTitle: post.postTitle.toLowerCase(),
      postContent: post.postContent.toLowerCase(),
    }));
    const updateResults = (e) => {
      query = e.target.value.toLowerCase().trim()
      resultsContainer.innerHTML = '';
      if (query.length < 2) return;

      const filtered = posts.filter(post =>
        post.postTitle.includes(query) ||
        post.postContent.includes(query)
      );

      const p = document.createElement('p');
      p.textContent = `${filtered.length} results found`
      resultsContainer.appendChild(p);

      if (filtered.length === 0) return;

      const results = document.createElement('ul');
      resultsContainer.appendChild(results);

      filtered.forEach(post => {
        const li = document.createElement('li');
        const a = document.createElement('a');
        a.href = `/${post.postRoute}`;
        a.textContent = `${post.postTitle} (${post.postDate})`;
        li.appendChild(a);
        results.appendChild(li);
      });
    };
    input.addEventListener('input', updateResults)
  });
